mod error;

use shared::containerless::storage;

use futures_retry::{FutureRetry, RetryPolicy};
use hyper::Response;
use reqwest;
use std::env;
use std::process::Stdio;
use std::time::Duration;
use tokio::io::AsyncWriteExt;
use tokio::{fs::File, process, task};
use warp::{http::StatusCode, Filter};

const MAX_INIT_PINGS: usize = 5;
const INIT_PING_INTERVAL_SECS: u64 = 1;

type WarpResult<T> = Result<T, warp::Rejection>;

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

async fn monitor_nodejs_process(function_name: &str, handle: process::Child) -> () {
    let exit_code = handle.await.expect("error waiting for nodejs process");
    match exit_code.code() {
        Some(code) => eprintln!(
            "MONITOR {}: nodejs process terminated with exit code {}",
            function_name, code
        ),
        None => eprintln!(
            "MONITOR {}: nodejs process terminated by signal",
            function_name
        ),
    }
    std::process::exit(1);
}

async fn initialize(function_name: String, tracing_enabled: bool) -> Result<(), error::Error> {
    let function_code = storage::get_internal(&function_name).await?;
    eprintln!(
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
        eprintln!(
            "INITIALIZE {}: js-transform stderr: {}",
            function_name,
            String::from_utf8_lossy(&output.stderr)
        );
        if output.status.success() == false {
            println!("{}", String::from_utf8_lossy(&output.stdout));
            eprintln!(
                "INITIALIZE {}: js-transform stdout: {}",
                function_name,
                String::from_utf8_lossy(&output.stdout)
            );
            return Err(error::Error::CompileError);
        }

        let mut traced = File::create("traced.js").await?;
        traced.write_all(&output.stdout).await?;
        eprintln!("INITIALIZE {}: trace compilation complete", function_name);
    }

    eprintln!("INITIALIZE {}: starting nodejs process", function_name);
    let nodejs_process = match tracing_enabled {
        false => process::Command::new("node")
            .arg("index.js")
            .arg("8081")
            .arg("disable-tracing")
            .spawn()?,
        true => process::Command::new("node")
            .arg("traced.js")
            .arg("8081")
            .spawn()?,
    };

    eprintln!(
        "INITIALIZE {}: waiting for http server to come up",
        function_name
    );
    wait_for_http_server().await?;
    eprintln!(
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
            eprintln!("{}", err);
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
    let function_name = env::var("FUNCTION_NAME").expect("envvar FUNCTION_NAME should be set");
    let function_mode = env::var("FUNCTION_MODE").expect("envvar FUNCTION_MODE should be set");
    let tracing_enabled = match function_mode.as_str() {
        "vanilla" => false,
        "tracing" => true,
        _ => panic!("envvar FUNCTION_MODE must be \"vanilla\" or \"tracing\""),
    };

    eprintln!(
        "UP {}: pod up (tracing enabled: {})",
        &function_name, tracing_enabled
    );

    if let Err(err) = initialize(function_name.clone(), tracing_enabled).await {
        eprintln!("Error during initialization: {}", err);
        std::process::exit(1);
    }
    let status_route = warp::path!("ready").and(warp::get()).and_then(status);
    let get_trace_route = warp::path!("trace").and(warp::get()).and_then(trace);
    let paths = status_route.or(get_trace_route);

    shared::net::serve_until_sigterm(paths, 8080).await;
    eprintln!(
        "DOWN {}: pod down (tracing enabled: {})",
        &function_name, tracing_enabled
    );
}
