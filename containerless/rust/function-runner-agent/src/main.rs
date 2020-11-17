mod error;

use std::marker::Unpin;
use futures_retry::{FutureRetry, RetryPolicy};
use hyper::Response;
use reqwest;
use std::env;
use std::process::Stdio;
use std::time::Duration;
use futures::prelude::*;
use tokio::io::{BufReader, AsyncRead, AsyncBufReadExt, AsyncWriteExt};
use tokio::{fs::File, process, task};
use warp::{http::StatusCode, Filter};
use log::{info, error, debug};
use tokio::time;

const MAX_INIT_PINGS: usize = 5;
const INIT_PING_INTERVAL_SECS: u64 = 1;
const KILL_DELAY: u64 = 5;

type WarpResult<T> = Result<T, warp::Rejection>;

/// Gives logs a change to be sent in the background.
async fn terminate_after_delay(code: i32) -> ! {
    time::delay_for(Duration::from_secs(KILL_DELAY)).await;
    std::process::exit(code);
}

async fn wait_for_http_server() -> Result<(), error::Error> {
    let mut tries = MAX_INIT_PINGS;
    let _resp = FutureRetry::new(
        move || reqwest::get("http://localhost:8081/readinessProbe"),
        move |err| {
            if tries == 0 {
                return RetryPolicy::ForwardError(err);
            }
            tries = tries - 1;
            return RetryPolicy::WaitRetry(Duration::from_secs(INIT_PING_INTERVAL_SECS));
        },
    )
    .await?;
    return Ok(());
}

async fn monitor_nodejs_process(function_name: &str, mut handle: process::Child) -> () {
    task::spawn(reflect_child_output_stream(handle.stdout.take().unwrap()));
    task::spawn(reflect_child_output_stream(handle.stderr.take().unwrap()));
    let exit_code = handle.await.expect("error waiting for nodejs process");
    match exit_code.code() {
        Some(code) => error!(target: "function-runner",
            "MONITOR {}: nodejs process terminated with exit code {}. Shutting down after {} seconds.",
            function_name, code, KILL_DELAY
        ),
        None => error!(target: "function-runner",
            "MONITOR {}: nodejs process terminated by signal. Shutting down after {} seconds.",
            function_name, KILL_DELAY
        ),
    }
    terminate_after_delay(1).await;
}

/// Reads lines from stdout / stderr and sends them to our log.
async fn reflect_child_output_stream(
    stdout_or_stderr: impl AsyncRead + Unpin) {
    let reader = BufReader::new(stdout_or_stderr);
    let mut lines = reader.lines();
    while let Some(Ok(line)) = lines.next().await {
        info!(target: "function-runner", "{}", line);
    }
}

async fn initialize(function_name: String, tracing_enabled: bool) -> Result<(), error::Error> {
    let resp = reqwest::get(&format!(
        "http://storage:8080/get_function/{}",
        &function_name
    ))
    .await?;

    if resp.status().as_u16() != 200 {
        return Err(error::Error::FileNotFound);
    }

    let function_code = resp.text().await?;
    info!(target: "function-runner",
        "INITIALIZE {}: downloaded function ({} bytes)",
        function_name,
        function_code.len()
    );

    // Write the serverless function to a file. This is needed whether or
    // not we are tracing.
    let mut vanilla = File::create("index.js").await?;
    vanilla.write(function_code.as_bytes()).await?;

    if tracing_enabled {
        let output = process::Command::new("./js-transform.sh")
            .arg("index.js")
            .stderr(Stdio::inherit())
            .output()
            .await?;
        info!(target: "function-runner",
            "INITIALIZE {}: js-transform stderr: {}",
            function_name,
            String::from_utf8_lossy(&output.stderr)
        );
        if output.status.success() == false {
            error!(target: "function-runner",
                "INITIALIZE {}: js-transform stdout: {}",
                function_name,
                String::from_utf8_lossy(&output.stdout)
            );
            return Err(error::Error::CompileError);
        }

        let mut traced = File::create("traced.js").await?;
        traced.write_all(&output.stdout).await?;
        info!(target: "function-runner", "INITIALIZE {}: trace compilation complete", function_name);
    }

    debug!(target: "function-runner", "INITIALIZE {}: starting nodejs process", function_name);
    let nodejs_process = match tracing_enabled {
        false => process::Command::new("node")
            .arg("index.js")
            .arg("8081")
            .arg("disable-tracing")
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()?,
        true => process::Command::new("node")
            .arg("traced.js")
            .arg("8081")
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()?,
    };    

    debug!(target: "function-runner", 
        "INITIALIZE {}: waiting for http server to come up",
        function_name
    );
    wait_for_http_server().await?;
    debug!(target: "function-runner", 
        "INITIALIZE {}: spawning child process to monitor function",
        function_name
    );

    task::spawn(async move { monitor_nodejs_process(&function_name, nodejs_process).await });
    return Ok(());
}

async fn status() -> WarpResult<impl warp::Reply> {
    return Ok(Response::builder()
        .status(200)
        .body("Function runner agent\n"));
}

async fn get_trace() -> reqwest::Result<String> {
    let resp = reqwest::get("http://127.0.0.1:8081/trace").await?;
    let body = resp.text().await?;
    return Ok(body);
}

async fn trace() -> WarpResult<impl warp::Reply> {
    let trace = get_trace().await;
    match trace {
        Err(err) => {
            error!(target: "function-runner", "{}", err);
            return Ok(hyper::Response::builder()
                .status(StatusCode::INTERNAL_SERVER_ERROR)
                .body("".to_owned()));
        }
        Ok(trace) => {
            return Ok(hyper::Response::builder().body(trace));
        }
    }
}

#[tokio::main]
async fn main() {
    shared::logger::init("http://controller-logger", 1);
    let function_name = env::var("FUNCTION_NAME").expect("envvar FUNCTION_NAME should be set");
    let function_mode = env::var("FUNCTION_MODE").expect("envvar FUNCTION_MODE should be set");
    let tracing_enabled = match function_mode.as_str() {
        "vanilla" => false,
        "tracing" => true,
        _ => panic!("envvar FUNCTION_MODE must be \"vanilla\" or \"tracing\""),
    };

    info!(target: "function-runner",
        "UP {}: pod up (tracing enabled: {})",
        &function_name, tracing_enabled
    );

    if let Err(err) = initialize(function_name.clone(), tracing_enabled).await {
        error!(target: "function-runner", "Error during initialization: {}", err);
        terminate_after_delay(1).await;
    }
    let status_route = warp::path!("ready").and(warp::get()).and_then(status);
    let get_trace_route = warp::path!("trace").and(warp::get()).and_then(trace);
    let paths = status_route.or(get_trace_route);

    shared::net::serve_until_sigterm(paths, 8080).await;
    info!(target: "function-runner",
        "DOWN {}: pod down (tracing enabled: {})",
        &function_name, tracing_enabled
    );
    terminate_after_delay(0).await;
}
