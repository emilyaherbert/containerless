#[cfg(test)]
use crate::trace_js::to_exp;
use crate::{
    types::Exp,
    verif::verify,
};
use crate::codegen;
use std::process::Command;
use std::process::Stdio;
use std::io::prelude::*;


/// An "end-to-end" test of decontainerization that runs a program in JS,
/// extracts its traces, compiles to Rust, and then runs the Rust program.
/// However, note that we do not run the JS in a container or start a
/// real web server for any of this. All I/O is via stdin and stdout.
///
/// # Arguments
///
/// * `filename` - The filename to use for the JavaScript code. Good for
///   debugging.
/// * `code` - The JavaScript code for the test.
/// * `js_requests` - The requests to make to JS, 1 request per line.
/// * `rs_requests` - The requests to make to Rust, 1 request per line.
///
/// # Result
///
/// The result is a vector of responses from the Rust code.
///
/// *Note*: At the moment, the requests are just the path.
fn test_end_to_end(
    filename: &str,
    code: &str,
    js_requests: &str,
    rs_requests: &str) -> Vec<String> {
    let json = to_exp(filename, code, js_requests);
    std::fs::remove_file(filename).expect("removing file");
    let exp = serde_json::from_str::<Exp>(&json)
        .unwrap_or_else(|exp| panic!("\n{:?} \nin \n{}", exp, &json));
    let verified = verify(&exp);
    codegen::codegen(&verified, "../containerless-scaffold/src/containerless.rs");
    let mut decontainerized = Command::new("cargo")
        .args(["run", "ignored-arg", "--testing"].iter())
        .current_dir("../containerless-scaffold")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("starting decontainerized function (in Rust)");
    decontainerized
        .stdin
        .as_mut()
        .expect("opening stdin")
        .write_all(rs_requests.as_bytes())
        .expect("failed to write requests");

    let exit = decontainerized
        .wait()
        .expect("running decontainerized function");
    assert!(exit.success(), "non-zero exit code from function");

    let mut stdout = String::new();
    decontainerized
        .stdout
        .unwrap()
        .read_to_string(&mut stdout)
        .expect("reading stdout");
        
    return stdout.split_terminator('\n').map(|s| s.to_string()).collect();
}

#[test]
pub fn trivial_fixed_response() {
    let result = test_end_to_end(
        "try_test.js",
        r#"
        let containerless = require("../../javascript/containerless");
        containerless.listen(function(req) {
            containerless.respond("Hello, world!");
        });"#,
        "input",
        "input");
    assert_eq!(result, vec!["Hello, world!"]);
}

#[test]
pub fn trivial_echo_path() {
    let result = test_end_to_end(
        "try_test.js",
        r#"
        let containerless = require("../../javascript/containerless");
        containerless.listen(function(req) {
            containerless.respond(req.path);
        });"#,
        "/apath",
        "/bpath");
    assert_eq!(result, ["/bpath"]);
}

#[test]
pub fn trivial_conditional_with_unknown() {
    let result = test_end_to_end(
        "trivial_conditional_with_unknown.js",
        r#"
        let containerless = require("../../javascript/containerless");
        containerless.listen(function(req) {
            if (req.path === 'hello') {
                containerless.respond("correct");
            }
            else {
                containerless.respond("wrong");
            }
        });
    "#,
        "hello",
        "hello"
    );

    assert_eq!(result, ["correct"]);
}

#[test]
pub fn nested_binops() {
    let result = test_end_to_end(
        "trivial_conditional_with_unknown.js",
        r#"
        let containerless = require("../../javascript/containerless");

        containerless.listen(function(req) {
            let x = 12;
            if(x > 2 && x < 15) {
                containerless.respond("yay!");
            } else {
                containerless.respond("boo!");
            }
        });
    "#,
        "hello",
        "hello"
    );

    assert_eq!(result, ["yay!"]);
}