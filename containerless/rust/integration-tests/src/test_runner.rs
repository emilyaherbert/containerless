use crate::error::Error;

use shared::common::*;
use shared::response::*;

use reqwest;
use serde_json::Value as JsonValue;
use std::process::Stdio;
use tokio::process::Command;
use std::time::{Instant, Duration};

struct TestRunner { }

impl TestRunner {
    async fn new() -> Self {
        TestRunner {}
    }

    pub async fn containerless_create(&self, name: &str, js_code: &str) -> Result<std::process::Output, Error> {
        let filename = format!("{}/_code.js", ROOT.as_str());
        std::fs::write(&filename, js_code)?;
        Ok(Command::new(format!("{}/debug/cli", ROOT.as_str()))
            .args(vec!("create", "-n", name, "-f", &filename))
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()
            .await?)
    }

    pub async fn containerless_delete(&self, name: &str) -> Result<std::process::Output, Error> {
        Ok(Command::new(format!("{}/debug/cli", ROOT.as_str()))
            .args(vec!("delete", "-n", name))
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()
            .await?)
    }

    pub async fn containerless_invoke(&self, name: &str, req: (&str, JsonValue)) -> Result<String, Error> {
        let (path, body) = req;
        let resp = reqwest::Client::new()
            .post(&format!("http://localhost/dispatcher/{}/{}", name, path))
            .json(&body)
            .send()
            .await?;
        response_into_result(resp.status().as_u16(), resp.text().await?).map_err(Error::Invoke)
    }

    pub async fn containerless_compile(&self, name: &str) -> Result<String, Error> {
        let resp = reqwest::Client::new()
            .post(&format!("http://localhost/dispatcher/compile/{}", name))
            .body("")
            .send()
            .await?;
        response_into_result(resp.status().as_u16(), resp.text().await?).map_err(Error::Compile)
    }

    pub async fn poll_for_dispatcher(&self) -> Result<(), Error> {
        let interval = Duration::from_secs(1);
        let timeout = Duration::from_secs(60);
        let end_time = Instant::now() + timeout;

        tokio::time::delay_for(interval).await;
        loop {
            let resp = reqwest::Client::new()
                .get("http://localhost/controller/ok_if_not_compiling")
                .send()
                .await?;
            match resp.status().as_u16() {
                200 => {
                    break;
                },
                _ => {}
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

    if let Err(err) = runner.containerless_create(name, js_code).await {
        error!(target: "integration-tests", "Error in the test runner: {:?}", err);
        if let Err(err) = runner.containerless_delete(name).await {
            error!(target: "integration-tests", "Error in the test runner: {:?}", err);
        }
        assert!(false, format!("{:?}", err));
        return results;
    }

    for req in js_requests.into_iter() {
        match runner.containerless_invoke(name, req).await {
            Ok(result) => results.push(result),
            Err(err) => {
                error!(target: "integration-tests", "Error in the test runner: {:?}", err);
                if let Err(err) = runner.containerless_delete(name).await {
                    error!(target: "integration-tests", "Error in the test runner: {:?}", err);
                }
                assert!(false, format!("{:?}", err));
                return results;
            }
        }
    }

    if let Err(err) = runner.containerless_compile(name).await {
        error!(target: "integration-tests", "Error in the test runner: {:?}", err);
        if let Err(err) = runner.containerless_delete(name).await {
            error!(target: "integration-tests", "Error in the test runner: {:?}", err);
        }
        assert!(false, format!("{:?}", err));
        return results;
    }

    if let Err(err) = runner.poll_for_dispatcher().await {
        error!(target: "integration-tests", "Error in the test runner: {:?}", err);
        if let Err(err) = runner.containerless_delete(name).await {
            error!(target: "integration-tests", "Error in the test runner: {:?}", err);
        }
        assert!(false, format!("{:?}", err));
        return results;
    }

    for req in rs_requests.into_iter() {
        match runner.containerless_invoke(name, req).await {
            Ok(result) => results.push(result),
            Err(err) => {
                error!(target: "integration-tests", "Error in the test runner: {:?}", err);
                if let Err(err) = runner.containerless_delete(name).await {
                    error!(target: "integration-tests", "Error in the test runner: {:?}", err);
                }
                assert!(false, format!("{:?}", err));
                return results;
            }
        }
    }

    if let Err(err) = runner.containerless_delete(name).await {
        error!(target: "integration-tests", "Error in the test runner: {:?}", err);
        if let Err(err) = runner.containerless_delete(name).await {
            error!(target: "integration-tests", "Error in the test runner: {:?}", err);
        }
        assert!(false, format!("{:?}", err));
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
