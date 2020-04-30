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
fn single_binop() {
    let results = run_test(
        "single_binop",
        r#"
        let containerless = require("containerless");
        containerless.listen(function(req) {
            let x = 12;
            if(x > 2) {
                containerless.respond("yay!");
            } else {
                containerless.respond("boo!");
            }
        });"#,
        vec![("/hello", json!({}))],
        vec![("/hello", json!({}))],
    );
    assert_eq!(results, vec!["yay!", "yay!"]);
}

#[test]
fn nested_binops() {
    let results = run_test(
        "nested_binops",
        r#"
        let containerless = require("containerless");
        containerless.listen(function(req) {
            let x = 12;
            if(x > 2 && x < 15) {
                containerless.respond("yay!");
            } else {
                containerless.respond("boo!");
            }
        });"#,
        vec![("/hello", json!({}))],
        vec![("/hello", json!({}))],
    );
    assert_eq!(results, vec!["yay!", "yay!"]);
}