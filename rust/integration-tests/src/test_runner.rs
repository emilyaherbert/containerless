use reqwest;
use serde_json::Value as JsonValue;
use std::fs;
use std::time::Duration;
use tokio::time::delay_for;

struct TestRunner {
    http_client: reqwest::Client,
    name: String,
}

impl TestRunner {
    fn new(name: String) -> Self {
        return TestRunner {
            http_client: reqwest::Client::new(),
            name,
        };
    }

    async fn poll_controller_for_compiled_ok(&self) -> bool {
        for _i in 0..40 {
            let resp = self
                .http_client
                .get(&format!(
                    "http://localhost/controller/get_status/{}",
                    self.name
                ))
                .body("")
                .send()
                .await
                .expect("checking compile status");
            let text = resp.text().await.expect("reading compile response body");
            match text.as_ref() {
                "Error" => return false,
                "Compiled" => return true,
                "Compiling" => (),
                "Unknown" => (),
                unexpected => panic!("unexpected status from compiler {}", unexpected),
            }
            delay_for(Duration::from_secs(1)).await;
        }
        return false;
    }

    async fn poll_dispatcher_for_decontainerized(&self) -> bool {
        for _i in 0..20 {
            let resp = self
                .http_client
                .get(&format!("http://localhost/dispatcher/mode/{}", self.name))
                .body("")
                .send()
                .await
                .expect("checking compile status");
            let text = resp.text().await.expect("reading compile response body");
            if text == "Decontainerized" {
                return true;
            }
            delay_for(Duration::from_secs(1)).await;
        }
        return false;
    }

    async fn send_post_request(
        &self,
        path_suffix: &str,
        body: JsonValue,
        expected_mode: &str,
    ) -> String {
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
            .map(|mode| mode.to_str().unwrap().to_string());
        let resp_body = resp.text().await.unwrap();
        assert_eq!(
            status, 200,
            "got response code {} with body {}",
            status, resp_body
        );
        assert_eq!(mode.as_deref(), Some(expected_mode));
        return resp_body;
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
    let runner = TestRunner::new(name.to_string());
    let js_path = format!("../../examples/{}.js", &name);
    fs::write(&js_path, js_code).expect(&format!("creating file {}", &js_path));

    let mut results = Vec::new();

    for (path_suffix, body) in js_requests.into_iter() {
        results.push(
            runner
                .send_post_request(&path_suffix, body, "tracing")
                .await,
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

    assert_eq!(runner.poll_controller_for_compiled_ok().await, true);
    assert_eq!(runner.poll_dispatcher_for_decontainerized().await, true);

    for (path_suffix, body) in rs_requests.into_iter() {
        results.push(
            runner
                .send_post_request(&path_suffix, body, "decontainerized")
                .await,
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
    let mut rt = tokio::runtime::Runtime::new().expect("creating Tokio runtime");
    return rt.block_on(run_test_async(name, js_code, js_requests, rs_requests));
}
