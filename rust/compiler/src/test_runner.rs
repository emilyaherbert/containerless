use super::codegen;
use super::verif::verify;
use serde_json::Value as JsonValue;
use std::fs::{self, File};
use std::io::prelude::*;
use std::process::Command;
use std::process::Stdio;

pub struct TestRunner {
    tracing_js: String,
    trace_json: String,
}

impl Drop for TestRunner {
    fn drop(&mut self) {
        if std::thread::panicking() {
            eprintln!(
                "Remember to delete the files {} and {}.",
                &self.tracing_js, &self.trace_json
            );
            return;
        }
        fs::remove_file(&self.tracing_js).unwrap();
        fs::remove_file(&self.trace_json).unwrap();
    }
}

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
impl TestRunner {
    pub fn new(tracing_js: &str) -> Self {
        TestRunner {
            tracing_js: tracing_js.to_string(),
            trace_json: format!("{}.json", tracing_js),
        }
    }

    pub fn test(
        &mut self,
        code: &str,
        js_requests: JsonValue,
        rs_requests: JsonValue,
    ) -> Vec<String> {
        let js_requests = serde_json::to_string_pretty(&js_requests)
            .expect("could not produce JSON string from js_requests");
        let rs_requests = serde_json::to_string_pretty(&rs_requests)
            .expect("could not produce JSON string from rs_requests");

        let mut js_transform = Command::new("node")
            .arg("../../javascript/js-transform")
            .stdin(Stdio::piped())
            .stdout(File::create(&self.tracing_js).expect("creating file"))
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
            .arg(&self.tracing_js)
            .arg("test")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .expect("starting decontainerized function");

        decontainerized_js
            .stdin
            .as_mut()
            .expect("opening stdin")
            .write_all(js_requests.as_bytes())
            .expect("failed to write requests");

        let exit = decontainerized_js
            .wait()
            .expect("running decontainerized function");
        assert!(exit.success(), "non-zero exit code from function");

        let mut trace_json = Vec::new();
        decontainerized_js
            .stdout
            .unwrap()
            .read_to_end(&mut trace_json)
            .expect("reading stdout");
        File::create(&self.trace_json)
            .unwrap()
            .write(&trace_json)
            .unwrap();

        let exp = serde_json::from_slice(&trace_json).expect("deserializing JSON trace");

        let verified = verify(&exp);
        codegen::codegen(&verified, "../containerless-scaffold/src/containerless.rs");
        let mut decontainerized = Command::new("cargo")
            .args(["run", "--", "--testing"].iter())
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

        return stdout
            .split_terminator('\n')
            .map(|s| s.to_string())
            .collect();
    }
}
