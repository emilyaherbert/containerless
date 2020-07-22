use k8s::Client as K8sClient;
use reqwest;
use serde_json::Value as JsonValue;
use shared::net::poll_url_with_timeout;
use std::fs;
use std::time::Duration;
use tokio::time::delay_for;

struct TestRunner {
    http_client: reqwest::Client,
    k8s_client: K8sClient,
    name: String,
}

impl TestRunner {
    async fn new(name: String) -> Self {
        let k8s_client = K8sClient::from_kubeconfig_file("containerless")
            .await
            .expect("creating k8s::Client");

        return TestRunner {
            http_client: reqwest::Client::new(),
            k8s_client,
            name,
        };
    }

    async fn poll_dispatcher_for_decontainerized(&self, previous_generation: usize) {
        loop {
            delay_for(Duration::from_secs(1)).await;
            let status = self
                .k8s_client
                .get_deployment_status("dispatcher")
                .await
                .unwrap();
            if status.replicas != 1 {
                continue;
            }
            if status.observed_generation == previous_generation {
                continue;
            }
            if status.observed_generation == previous_generation + 1 {
                return;
            }
            panic!(
                "observed generation is {}, but previous generation was {}",
                status.observed_generation, previous_generation
            );
        }
    }

    async fn send_post_request(
        &self,
        path_suffix: &str,
        body: JsonValue,
        expected_mode: &str,
    ) -> Result<String, String> {
        let url = reqwest::Url::parse(&format!(
            "http://localhost/dispatcher/{}/{}",
            self.name, path_suffix
        ))
        .expect("parsing URL");
        let body = serde_json::to_vec(&body).expect("serializing JSON body");
        let resp = self
            .http_client
            .post(url)
            .body(body)
            .send()
            .await
            .expect("sending request");
        let status = resp.status();
        let mode = resp
            .headers()
            .get("X-Containerless-Mode")
            .map(|mode| mode.to_str().unwrap().to_string())
            .unwrap_or(String::new());
        let resp_body = resp.text().await.unwrap();
        if status != 200 {
            return Err(format!(
                "got response code {} with body {} and mode {}",
                status, resp_body, mode
            ));
        }
        if mode != expected_mode {
            return Err(format!("response had mode {:?}", mode));
        }
        return Ok(resp_body);
    }
}

pub async fn run_test_async(
    name: &str,
    js_code: &str,
    js_requests: Vec<(&str, JsonValue)>,
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
    let runner = TestRunner::new(name.to_string()).await;
    let js_path = format!("../../examples/{}.js", &name);
    fs::write(&js_path, js_code).expect(&format!("creating file {}", &js_path));

    let mut results = Vec::new();

    let dispatcher_generation = runner
        .k8s_client
        .get_deployment_status("dispatcher")
        .await
        .unwrap()
        .observed_generation;

    for (path_suffix, body) in js_requests.into_iter() {
        results.push(
            runner
                .send_post_request(&path_suffix, body, "tracing")
                .await
                .unwrap(),
        );
    }

    assert_eq!(
        runner
            .http_client
            .post(&format!("http://localhost/dispatcher/compile/{}", name))
            .body("")
            .send()
            .await
            .expect("sending compile request")
            .status(),
        200
    );

    poll_url_with_timeout(
        &runner.http_client,
        "http://localhost/controller/ok_if_not_compiling",
        Duration::from_secs(1),
        Duration::from_secs(60),
    )
    .await
    .expect("compiler took too long");

    runner
        .poll_dispatcher_for_decontainerized(dispatcher_generation)
        .await;
    delay_for(Duration::from_secs(1)).await;

    for (path_suffix, body) in rs_requests.into_iter() {
        results.push(
            runner
                .send_post_request(&path_suffix, body, "decontainerized")
                .await
                .unwrap(),
        );
    }

    return results;
}

#[allow(unused)]
pub fn run_test(
    name: &str,
    js_code: &str,
    js_requests: Vec<(&str, JsonValue)>,
    rs_requests: Vec<(&str, JsonValue)>,
) -> Vec<String> {
    if name.contains("-") || name.contains("_") {
        panic!("The function name contains an invalid characters.");
    }

    let mut rt = tokio::runtime::Runtime::new().expect("creating Tokio runtime");
    return rt.block_on(run_test_async(name, js_code, js_requests, rs_requests));
}
