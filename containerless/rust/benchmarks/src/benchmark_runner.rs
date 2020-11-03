use crate::error::Error;

use shared::common::*;
use shared::containerless::cli;

use tokio::process::Command;
use std::process::Stdio;

use std::fs::File;
use std::io::prelude::*;

#[derive(Clone)]
pub struct WrkOptions {
    pub connections: usize,
    pub duration: usize,
    pub threads: usize,
    pub script_filename: Option<String>,
    pub save_wrk_output: bool
}

/*
impl std::fmt::Display for WrkOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}_connections_{}_duration_{}_threads", self.connections, self.duration, self.threads)
    }
}
*/

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

async fn run_one_mode(name: &str, js_code: &str, url: &str, wrk_options: WrkOptions, containers_only: bool, output_path: &str) -> Option<String> {

    let run_name = format!("{}_{}_{}_{}_{}",
        name,
        wrk_options.connections,
        wrk_options.duration,
        wrk_options.threads,
        if containers_only { "vanilla" } else { "tracing" });

    // Create output path for data
    Command::new("mkdir")
        .args(vec!("--parents", &output_path))
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .await
        .expect("Could not make output path.");

    // Create the function in Containerless
    if let Err(err) = cli::containerless_create(name, js_code, containers_only).await {
        assert!(false, format!("{:?}", err));
    }

    // Hammer the function using wrk
    let (stdout, _stderr) = wrk_run(url, wrk_options.clone()).await
        .expect("Invoking with wrk failed.");

    // If applicable, save the output from wrk
    if wrk_options.save_wrk_output {
        let mut file = File::create(format!("{}/{}.csv", output_path, run_name)).unwrap();
        file.write_all(stdout.as_bytes())
            .expect("Could not write to file.");
    }

    Some("Done!".to_string())
}

async fn run_benchmark_async(name:&str, js_code: &str, url: &str, wrk_options: WrkOptions) -> Option<String> {

    let output_path_vanilla = format!("{}/../benchmarks/data/latency/{}/vanilla", ROOT.as_str(), name);
    run_one_mode(name, js_code, url, wrk_options.clone(), true, &output_path_vanilla).await?;

    let output_path_tracing = format!("{}/../benchmarks/data/latency/{}/tracing", ROOT.as_str(), name);
    run_one_mode(name, js_code, url, wrk_options, false, &output_path_tracing).await
}

#[allow(unused)]
pub fn run_benchmark(name:&str, js_code: &str, url: &str, wrk_options: WrkOptions) -> Option<String> {
    let mut rt = tokio::runtime::Runtime::new().expect("creating Tokio runtime");
    rt.block_on(run_benchmark_async(name, js_code, url, wrk_options))
}