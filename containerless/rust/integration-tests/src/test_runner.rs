use crate::error::Error;

use shared::containerless::cli;

use serde_json::Value as JsonValue;
use std::time::{Duration, Instant};

pub async fn poll_for_new_dispatcher(old_version: usize) -> Result<(), Error> {
    let interval = Duration::from_secs(1);
    let timeout = Duration::from_secs(60);
    let end_time = Instant::now() + timeout;

    tokio::time::delay_for(interval).await;
    loop {
        let output = cli::containerless_get_dispatcher_version().await?;
        let status = output.status;
        let stdout = String::from_utf8(output.stdout)?;
        let _stderr = String::from_utf8(output.stderr)?;
        let next_version = stdout.trim().parse::<usize>()?;
        if status.success() && next_version == (old_version + 1) {
            break;
        }
        tokio::time::delay_for(interval).await;
        if Instant::now() >= end_time {
            return Err(Error::Timeout);
        }
    }
    tokio::time::delay_for(interval).await;

    return Ok(());
}

pub async fn run_test_async(
    name: &str, js_code: &str, js_requests: Vec<(&str, JsonValue)>,
    rs_requests: Vec<(&str, JsonValue)>,
) -> Vec<String> {
    assert!(
        js_requests.len() > 0,
        "expected at least one pre-tracing request"
    );
    assert!(
        rs_requests.len() > 0,
        "expected at least one post-tracing request"
    );
    let mut results = Vec::new();

    // Retrieve the old dispatcher version
    let dispatcher_version = cli::containerless_get_dispatcher_version().await;
    if let Err(err) = dispatcher_version {
        eprintln!("fail in the test runner: {:?}", err);
        return results;
    }
    let old_dispatcher_version = String::from_utf8(dispatcher_version.unwrap().stdout)
        .unwrap()
        .trim()
        .parse::<usize>()
        .unwrap();

    // Create the function in Containerless
    if let Err(err) = cli::containerless_create(name, js_code, false).await {
        assert!(false, format!("{:?}", err));
    }

    // Send requests to the containerized version
    for req in js_requests.into_iter() {
        let result = cli::containerless_invoke(name, req).await
            .expect("invoke JavaScript failed");
        results.push(result);
    }

    // Have containerless compile the function and redeploy the dispatcher
    cli::containerless_compile(name).await.expect("compile failed");

    // Poll for the new dispatcher
    poll_for_new_dispatcher(old_dispatcher_version).await
        .expect("deadline exceeded waiting for new dispatcher");

    // Send requests to the decontainerized version
    for req in rs_requests.into_iter() {
        let result = cli::containerless_invoke(name, req).await
            .expect("invoke Rust failed");
        results.push(result);
    }
    
    return results;
}

#[allow(unused)]
pub fn run_test(
    name: &str, js_code: &str, js_requests: Vec<(&str, JsonValue)>,
    rs_requests: Vec<(&str, JsonValue)>,
) -> Vec<String> {
    if name.contains("-") || name.contains("_") {
        panic!("The function name contains an invalid characters.");
    }

    let mut rt = tokio::runtime::Runtime::new().expect("creating Tokio runtime");
    return rt.block_on(run_test_async(name, js_code, js_requests, rs_requests));
}
