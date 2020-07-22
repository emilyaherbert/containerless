/**
 * This module is a small shim to the JavaScript tracing code.
 */
use std::fs;
use std::path::Path;
use std::fs::File;
use std::io::prelude::*;
use std::process::Command;
use std::process::Stdio;
use tempfile::NamedTempFile;

pub fn to_exp(filename: &str, code: &str, requests: &str) -> String {
    let f = File::create(filename).expect("creating file");
    let mut js_transform = Command::new("node")
        .arg("../../javascript/js-transform")
        .stdin(Stdio::piped())
        .stdout(f)
        .spawn()
        .expect("starting js-transform");
    js_transform
        .stdin
        .as_mut()
        .expect("opening stdin")
        .write_all(code.as_bytes())
        .expect("failed to write to JS file");
    let exit = js_transform.wait().expect("running js-transform");
    assert!(exit.success());

    let mut decontainerized_js = Command::new("node")
        .arg(filename)
        .arg("test")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("starting decontainerized function");

    decontainerized_js
        .stdin
        .as_mut()
        .expect("opening stdin")
        .write_all(requests.as_bytes())
        .expect("failed to write requests");

    let exit = decontainerized_js
        .wait()
        .expect("running decontainerized function");
    assert!(exit.success(), "non-zero exit code from function");

    let mut stdout = String::new();
    decontainerized_js
        .stdout
        .unwrap()
        .read_to_string(&mut stdout)
        .expect("reading stdout");
    return stdout;
}

pub fn trace_with_files(file: &str, input: &str) -> String {
    let parent_path = Path::new(file).parent().map(|p| p.to_str().unwrap()).unwrap_or(".");
    let untransformed_code =
        fs::read_to_string(file).expect(&format!("could not read file {}", file));
    let requests = fs::read_to_string(input).expect(&format!("could not read file {}", input));
    let transformed_file = NamedTempFile::new_in(parent_path).expect("could not create temporary file");
    let exp = to_exp(
        transformed_file.path().to_str().unwrap(),
        &untransformed_code,
        &requests,
    );
    return exp;
}
