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

#[test]
fn for_test() {
    let results = run_test(
        "fortest",
        r#"
        let containerless = require("containerless");
        containerless.listen(function(req) {
            let n = req.body.n;
            let x = 0;
            for(let i=0; i<n; i++) {
                x = x + 1;
            }
            containerless.respond("Done!");
        });"#,
        vec![("/hello", json!({"n": 2 }))],
        vec![("/hello", json!({"n": 2 }))],
    );
    assert_eq!(results, vec!["Done!", "Done!"]);
}

#[test]
fn while_test() {
    let results = run_test(
        "whiletest",
        r#"
        let containerless = require("containerless");
        containerless.listen(function(req) {
            let n = req.body.n;
            let x = 0;
            while(n > 0) {
                console.error(n);
                x = x + 1;
                n = n - 1;
            }
            containerless.respond("Done!");
        });"#,
        vec![("/hello", json!({"n": 2 }))],
        vec![("/hello", json!({"n": 2 }))],
    );
    assert_eq!(results, vec!["Done!", "Done!"]);
}

#[test]
fn array_pop() {
    let results = run_test(
        "arraypop",
        r#"
        let containerless = require("containerless");
        containerless.listen(function(req) {
            let x = [0];
            for(let i=0; i<x.length; i++) {
                x.pop();
            }
            containerless.respond("Done!");
        });"#,
        vec![("/hello", json!({}))],
        vec![("/hello", json!({}))],
    );
    assert_eq!(results, vec!["Done!", "Done!"]);
}

#[test]
fn else_before_if() {
    let results = run_test(
        "elsebeforeif",
        r#"
        let containerless = require("containerless");
        containerless.listen(function(req) {
            let x = req.body.x;
            if(x > 10) {
                containerless.respond("42");
                return;
            }
            containerless.respond("24");
        });"#,
        vec![("/hello", json!({ "x": 1 })), ("/hello", json!({ "x": 11 }))],
        vec![("/hello", json!({ "x": 11 }))],
    );
    assert_eq!(results, vec!["24", "42", "42"]);
}