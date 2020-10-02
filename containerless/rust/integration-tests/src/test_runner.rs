use crate::error::Error;

use shared::common::*;
use shared::cli;

use serde_json::Value as JsonValue;
use std::time::{Instant, Duration};

struct TestRunner { }

impl TestRunner {
    async fn new() -> Self {
        TestRunner {}
    }

    pub async fn poll_for_new_dispatcher(&self, old_version: usize) -> Result<(), Error> {
        let interval = Duration::from_secs(1);
        let timeout = Duration::from_secs(60);
        let end_time = Instant::now() + timeout;

        tokio::time::delay_for(interval).await;
        loop {
            // TODO(emily): Figure out better way to do this.
            let res: Result<std::process::Output, Error> = cli::containerless_get_dispatcher_version().await;
            let output = res?;
            let status = output.status;
            let stdout = String::from_utf8(output.stdout)?;
            let _stderr = String::from_utf8(output.stderr)?;
            let next_version = stdout.trim().parse::<usize>()?;
            if status.success() && next_version == (old_version+1) {
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
}

pub async fn run_test_async(name: &str, js_code: &str, js_requests: Vec<(&str, JsonValue)>, rs_requests: Vec<(&str, JsonValue)>) -> Vec<String> {
    fn fail(err: Error) {
        error!(target: "integration-tests", "Error in the test runner: {:?}", err);
        assert!(false, format!("{:?}", err));
    }

    async fn fail_and_delete(name: &str, err: Error) {
        error!(target: "integration-tests", "Error in the test runner: {:?}", err);
        let d: Result<std::process::Output, Error> = cli::containerless_delete(name).await;
        if let Err(err) = d {
            error!(target: "integration-tests", "Error in the test runner: {:?}", err);
        }
        assert!(false, format!("{:?}", err));
    }

    assert!(
        js_requests.len() > 0,
        "expected at least one pre-tracing request"
    );
    assert!(
        rs_requests.len() > 0,
        "expected at least one post-tracing request"
    );
    let runner = TestRunner::new().await;
    let mut results = Vec::new();

    // Retrieve the old dispatcher version
    let dispatcher_version = cli::containerless_get_dispatcher_version().await;
    if let Err(err) = dispatcher_version {
        fail(err);
        return results;
    }
    let old_dispatcher_version = String::from_utf8(dispatcher_version.unwrap().stdout)
        .unwrap()
        .trim()
        .parse::<usize>()
        .unwrap();

    // Create the function in Containerless
    if let Err(err) = cli::containerless_create(name, js_code).await {
        fail_and_delete(name, err).await;
        return results;
    }

    // Send requests to the containerized version
    for req in js_requests.into_iter() {
        match cli::containerless_invoke(name, req, |s| Error::Invoke(s)).await {
            Ok(result) => results.push(result),
            Err(err) => {
                fail_and_delete(name, err).await;
                return results;
            }
        }
    }

    // Have containerless compile the function and redeploy the dispatcher
    if let Err(err) = cli::containerless_compile(name).await {
        fail_and_delete(name, err).await;
        return results;
    }

    // Poll for the new dispatcher
    if let Err(err) = runner.poll_for_new_dispatcher(old_dispatcher_version).await {
        fail_and_delete(name, err).await;
        return results;
    }

    // Send requests to the decontainerized version
    for req in rs_requests.into_iter() {
        match cli::containerless_invoke(name, req, |s| Error::Invoke(s)).await {
            Ok(result) => results.push(result),
            Err(err) => {
                fail_and_delete(name, err).await;
                return results;
            }
        }
    }

    // Delete everything!
    if let Err(err) = cli::containerless_delete(name).await {
        fail_and_delete(name, err).await;
        return results;
    }

    results
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
