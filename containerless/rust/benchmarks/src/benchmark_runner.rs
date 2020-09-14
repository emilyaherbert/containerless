use crate::error::Error;

use shared::common::*;

use tokio::process::Command;
use std::process::Stdio;

use std::fs::File;
use std::io::prelude::*;

#[derive(Clone)]
pub struct WrkOptions {
    pub connections: usize,
    pub duration: usize,
    pub threads: usize
}

impl std::fmt::Display for WrkOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}_connections_{}_duration_{}_threads", self.connections, self.duration, self.threads)
    }
}

async fn wrk_run(url: &str, wrk_options: WrkOptions) -> Result<(String, String), Error> {
    let output = Command::new(format!("{}/../benchmarks/wrk/wrk", ROOT.as_str()))
        .args(vec![
            "-c",
            &wrk_options.connections.to_string(),
            "-d",
            &wrk_options.duration.to_string(),
            "-t",
            &wrk_options.threads.to_string(),
            url,
            "--supress",
            "--per_req"
        ])
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .await?;
    let stdout = String::from_utf8(output.stdout)?;
    let stderr = String::from_utf8(output.stderr)?;
    Ok((stdout, stderr))
}

async fn run_benchmark_async(name:&str, url: &str, wrk_options: WrkOptions) -> String {
    fn fail(err: Error) {
        error!(target: "benchmarks", "Error in the benchmark runner: {:?}", err);
        assert!(false, format!("{:?}", err));
    }

    // Hammer the function using wrk
    let wrk_output = wrk_run(url, wrk_options.clone()).await;
    if let Err(err) = wrk_output {
        fail(err);
        return "".to_string();
    }
    let (stdout, _stderr) = wrk_output.unwrap();

    // Save the results returned from wrk
    let mut file = File::create(format!("{}/../benchmarks/data/{}_{}.csv", ROOT.as_str(), name, wrk_options)).unwrap();
    if let Err(err) = file.write_all(stdout.as_bytes()) {
        fail(Error::IO(err));
        return "".to_string();
    }

    return "Done!".to_string();
}

#[allow(unused)]
pub fn run_benchmark(name:&str, url: &str, wrk_options: WrkOptions) -> String {
    let mut rt = tokio::runtime::Runtime::new().expect("creating Tokio runtime");
    return rt.block_on(run_benchmark_async(name, url, wrk_options));
}