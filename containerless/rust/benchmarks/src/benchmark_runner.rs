use crate::error::Error;

use shared::common::*;
use shared::cli;

use tokio::process::Command;
use std::process::Stdio;

use std::fs::File;
use std::io::prelude::*;

#[derive(Clone)]
pub struct WrkOptions {
    pub connections: usize,
    pub duration: usize,
    pub threads: usize,
    pub script_filename: Option<String>
}

impl std::fmt::Display for WrkOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}_connections_{}_duration_{}_threads", self.connections, self.duration, self.threads)
    }
}

#[allow(unused_assignments)]
async fn wrk_run(url: &str, wrk_options: WrkOptions) -> Result<(String, String), Error> {
    let c = wrk_options.connections.to_string();
    let d = wrk_options.duration.to_string();
    let t = wrk_options.threads.to_string();
    let mut args = vec![
        "-c",
        &c,
        "-d",
        &d,
        "-t",
        &t,
        url,
        "--supress",
        "--per_req"
    ];
    // rust strings suck
    let mut f = "".to_string();
    if let Some(filename) = wrk_options.script_filename {
        args.push("-s");
        f = format!("{}/../benchmarks/wrk_scripts/{}", ROOT.as_str(), filename);
        args.push(&f);
    }
    let output = Command::new(format!("{}/../benchmarks/wrk/wrk", ROOT.as_str()))
        .args(args)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .await?;
    let stdout = String::from_utf8(output.stdout)?;
    let stderr = String::from_utf8(output.stderr)?;
    Ok((stdout, stderr))
}

async fn run_benchmark_async(name:&str, js_code: &str, url: &str, wrk_options: WrkOptions) -> String {
    fn fail(err: Error) {
        error!(target: "benchmarks", "Error in the benchmark runner: {:?}", err);
        assert!(false, format!("{:?}", err));
    }

    async fn fail_and_delete(name: &str, err: Error) {
        error!(target: "benchmarks", "Error in the test runner: {:?}", err);
        let d: Result<std::process::Output, Error> = cli::containerless_delete(name).await;
        if let Err(err) = d {
            error!(target: "benchmarks", "Error in the test runner: {:?}", err);
        }
        assert!(false, format!("{:?}", err));
    }

    let empty = "".to_string();

    // Create the function in Containerless
    if let Err(err) = cli::containerless_create(name, js_code).await {
        fail_and_delete(name, err).await;
        return empty;
    }

    // Hammer the function using wrk
    let wrk_output = wrk_run(url, wrk_options.clone()).await;
    if let Err(err) = wrk_output {
        fail_and_delete(name, err).await;
        return empty;
    }
    let (stdout, _stderr) = wrk_output.unwrap();

    // Delete everything!
    if let Err(err) = cli::containerless_delete(name).await {
        fail_and_delete(name, err).await;
        return empty;
    }

    // Create output path for data
    let output_path_tracing = format!("{}/../benchmarks/data/latency/{}/tracing", ROOT.as_str(), name);
    if let Err(err) = Command::new("mkdir")
        .args(vec!("--parents", &output_path_tracing))
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .await {
            fail(Error::IO(err));
            return empty;
        }

    // Save the results returned from wrk
    let mut file = File::create(format!("{}/{}_{}.csv", output_path_tracing, name, wrk_options)).unwrap();
    if let Err(err) = file.write_all(stdout.as_bytes()) {
        fail(Error::IO(err));
        return empty;
    }

    return "Done!".to_string();
}

#[allow(unused)]
pub fn run_benchmark(name:&str, js_code: &str, url: &str, wrk_options: WrkOptions) -> String {
    let mut rt = tokio::runtime::Runtime::new().expect("creating Tokio runtime");
    return rt.block_on(run_benchmark_async(name, js_code, url, wrk_options));
}