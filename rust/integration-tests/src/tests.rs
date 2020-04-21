#![cfg(test)]
use super::test_runner::run_test;
use serde_json::json;

#[tokio::test]
async fn trivial_fixed_response() {
    let results = run_test(
        "trivialfixedresponse",
        r#"
        let containerless = require("containerless");
        containerless.listen(function(req) {
            containerless.respond("Hello, world!");
        });"#,
        vec![
            ("/hello", json!({}))
        ],
        vec![
            ("/hello", json!({}))
    ]).await;

    assert_eq!(results, vec!["Hello, world!", "Hello, world!"]);
}
