#![cfg(test)]
use super::test_runner::run_test;
use serde_json::json;

#[test]
fn trivial_fixed_response() {
    let results = run_test(
        "trivialfixedresponse",
        r#"
        let containerless = require("containerless");
        containerless.listen(function(req) {
            containerless.respond("Hello, world!");
        });"#,
        vec![("/hello", json!({}))],
        vec![("/hello", json!({}))],
    );

    assert_eq!(results, vec!["Hello, world!", "Hello, world!"]);
}

#[test]
fn loops() {
    let results = run_test(
        "loops",
        r#"
        let containerless = require("containerless");
        containerless.listen(function(req) {
            let arr = req.body.arr;
            let count = 0;
            for(let i=0; i<arr.length; i++) {
                count = count+1;
            }
            containerless.respond(count);
        });"#,
        vec![("/hello", json!({ "arr": [1, 2, 3] }))],
        vec![("/hello", json!({ "arr": [1, 2, 3] }))],
    );
    assert_eq!(results, vec!["3", "3"]);
}

#[test]
fn yay() {
    let results = run_test(
        "yay",
        r#"
        let containerless = require("containerless");
        containerless.listen(function(req) {
            containerless.respond("yay!");
        });"#,
        vec![("/hello", json!({}))],
        vec![("/hello", json!({}))],
    );
    assert_eq!(results, vec!["yay!", "yay!"]);
}

#[test]
fn frivolous_if() {
    let results = run_test(
        "frivolousif",
        r#"
        let containerless = require("containerless");
        containerless.listen(function(req) {
            let x = 12;
            if(x > 2) {
                x = 11;
            } else {
                x = 13;
            }
            containerless.respond("hi");
        });"#,
        vec![("/hello", json!({}))],
        vec![("/hello", json!({}))],
    );
    assert_eq!(results, vec!["hi", "hi"]);
}

#[test]
#[should_panic]
fn bad_name() {
    let results = run_test(
        "bad_name",
        r#"
        let containerless = require("containerless");
        containerless.listen(function(req) {
            containerless.respond("you won't see this");
        });"#,
        vec![("/hello", json!({}))],
        vec![("/hello", json!({}))],
    );
    assert_eq!(results, vec!["you won't see this", "you won't see this"]);
}