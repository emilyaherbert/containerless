mod error;
mod state;

use crate::state::State;
use futures_retry::{FutureRetry, RetryPolicy};
use reqwest;
use serde::Deserialize;
use state::StateHandle;
use std::process::Stdio;
use std::time::Duration;
use tokio::io::AsyncWriteExt;
use tokio::{fs::File, process, task};
use warp::{http::StatusCode, Filter};

const MAX_INIT_PINGS: usize = 5;
const INIT_PING_INTERVAL_SECS: u64 = 1;

#[derive(Deserialize, PartialEq)]
enum Mode {
    Tracing,
    Vanilla,
}

#[derive(Deserialize)]
struct Init {
    code: String,
    mode: Mode,
}

type WarpResult<T> = Result<T, warp::Rejection>;

async fn wait_for_http_server() -> Result<(), error::Error> {
    let mut tries = MAX_INIT_PINGS;
    let _resp = FutureRetry::new(
        move || reqwest::get("http://127.0.0.1:8081/ping"),
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

async fn monitor_nodejs_process(handle: process::Child, state: StateHandle) -> () {
    let exit_code = handle.await.expect("error waiting for nodejs process");
    match exit_code.code() {
        Some(code) => eprintln!("Node process terminated with exit code {}", code),
        None => eprintln!("Node process terminated by signal."),
    }
    state.set_runtime_error().await;
}

async fn compile_and_start_serverless(init: Init, state: StateHandle) -> Result<(), error::Error> {
    // Write the serverless function to a file. This is needed whether or
    // not we are tracing.
    let mut vanilla = File::create("index.js").await?;
    vanilla.write(init.code.as_bytes()).await?;

    if let Mode::Tracing = init.mode {
        let output = process::Command::new("./js-transform.sh")
            .arg("index.js")
            .stderr(Stdio::inherit())
            .output()
            .await?;
        if output.status.success() == false {
            println!("{}", String::from_utf8_lossy(&output.stdout));
            eprintln!("js-transform aborted with exit code {:?}", output.status);
            return Err(error::Error::CompileError);
        }

        let mut traced = File::create("traced.js").await?;
        traced.write_all(&output.stdout).await?;
    }

    let nodejs_process = match init.mode {
        Mode::Vanilla => process::Command::new("node")
            .arg("index.js")
            .arg("8081")
            .arg("disable-tracing")
            .spawn()?,
        Mode::Tracing => process::Command::new("node")
            .arg("traced.js")
            .arg("8081")
            .spawn()?,
    };

    wait_for_http_server().await?;
    task::spawn(async move { monitor_nodejs_process(nodejs_process, state).await });
    return Ok(());
}

async fn init(init: Init, state: StateHandle) -> WarpResult<impl warp::Reply> {
    if state.set_compiling().await == false {
        return Ok(StatusCode::BAD_REQUEST);
    }
    // We have atomically the state to compiling, so concurrent /init calls
    // will fail. This matters because compile_and_start_serverless writes
    // files to disk, and it is not thread-safe.
    let state = state.clone();
    let _ = task::spawn(async move {
        match compile_and_start_serverless(init, state.clone()).await {
            Err(err) => {
                eprintln!("Compile error: {}", err);
                state.set_compile_error().await
            }
            Ok(()) => state.set_running().await,
        }
    });
    // This does not mean that compiling succeeded. The client has to poll
    // /status to see if compiling suceeded.
    return Ok(StatusCode::OK);
}

async fn status(state: StateHandle) -> WarpResult<impl warp::Reply> {
    return Ok(warp::reply::json(&state.get_state().await));
}

async fn get_trace() -> reqwest::Result<String> {
    let resp = reqwest::get("http://127.0.0.1:8081/trace").await?;
    let body = resp.text().await?;
    return Ok(body);
}

async fn trace(state: StateHandle) -> WarpResult<impl warp::Reply> {
    if state.get_state().await != State::Running {
        return Ok(hyper::Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body("".to_owned()));
    }
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
    let state = StateHandle::new();
    let state = warp::any().map(move || state.clone());

    let init_route = warp::path!("init")
        .and(warp::post())
        .and(warp::body::json())
        .and(state.clone())
        .and_then(init);

    let status_route = warp::path!("status")
        .and(warp::get())
        .and(state.clone())
        .and_then(status);

    let get_trace_route = warp::path!("trace")
        .and(warp::get())
        .and(state.clone())
        .and_then(trace);

    let paths = init_route.or(status_route).or(get_trace_route);

    warp::serve(paths).run(([127, 0, 0, 1], 8080)).await;
    println!("Function Runner Agent terminated");
}
