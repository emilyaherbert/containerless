use bytes;
use hyper::Response;
use warp::Filter;

mod common;
mod compiler;
mod graceful_sigterm;
mod trace_compiler;

use common::*;
use graceful_sigterm::handle_sigterm;

async fn ready() -> Result<impl warp::Reply, warp::Rejection> {
    return Ok(Response::builder().status(200).body("Controller agent\n"));
}

async fn recv_trace(
    name: String,
    trace: bytes::Bytes,
    compiler: compiler::CompilerHandle,
) -> Result<impl warp::Reply, warp::Rejection> {
    info!(target: "controller", "received trace for {} ({} bytes)", &name, trace.len());
    compiler.compile(name, trace);
    return Ok(Response::builder().status(200).body(""));
}

#[tokio::main]
async fn main() {
    env_logger::init();
    info!(target: "controller", "Started Controller");
    let compiler = compiler::start_compiler_task();
    let compiler2 = compiler.clone();

    let ready_route = warp::path!("ready").and(warp::get()).and_then(ready);
    let download_dispatcher_route =
        warp::path("download_dispatcher")
            .and(warp::get())
            .and(warp::fs::file(
                "/src/dispatcher-agent/target/debug/dispatcher-agent",
            ));
    let recv_trace_route = warp::path!("recv_trace" / String)
        .and(warp::body::bytes())
        .and(warp::post())
        .and(warp::any().map(move || compiler.clone()))
        .and_then(recv_trace);

    let paths = ready_route
        .or(download_dispatcher_route)
        .or(recv_trace_route);

    let (_addr, server) = warp::serve(paths)
        .bind_with_graceful_shutdown(([0, 0, 0, 0], 80), suppress_and_log_err(handle_sigterm(compiler2)));
    server.await;
}
