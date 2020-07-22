use bytes;
use hyper::Response;
use warp::Filter;
#[macro_use]
extern crate lazy_static;

mod common;
mod compiler;
mod graceful_sigterm;
mod trace_compiler;

use common::*;
use graceful_sigterm::handle_sigterm;
use compiler::Compiler;

async fn ready() -> Result<impl warp::Reply, warp::Rejection> {
    return Ok(Response::builder().status(200).body("Controller agent\n"));
}

async fn recv_trace(
    name: String,
    trace: bytes::Bytes,
    compiler: Arc<Compiler>,
) -> Result<impl warp::Reply, warp::Rejection> {
    info!(target: "controller", "received trace for {} ({} bytes)", &name, trace.len());
    compiler.compile(name, trace);
    return Ok(Response::builder().status(200).body(""));
}

async fn ok_if_not_compiling_handler(
    compiler: Arc<Compiler>,
) -> Result<impl warp::Reply, warp::Rejection> {
    return Ok(compiler.ok_if_not_compiling());
}

async fn restart_dispatcher_handler(
    compiler: Arc<Compiler>,
    ) -> Result<impl warp::Reply, warp::Rejection> {
    return Ok(compiler.recompile_dispatcher().await);
}

async fn reset_dispatcher_handler(
    compiler: Arc<Compiler>,
    ) -> Result<impl warp::Reply, warp::Rejection> {
    return Ok(compiler.reset_dispatcher().await);
}

#[tokio::main]
async fn main() {
    shared::rsyslog::init_using_env();

    let k8s_client = Arc::new(k8s::Client::from_kubeconfig_file(NAMESPACE)
        .await
        .expect("creating k8s::Client"));

    let compiler = Compiler::new();
    assert!(compiler.cargo_build(None).await, "initial cargo build failed");
    
    k8s_client.new_deployment(compiler::dispatcher_deployment_spec(0))
        .await
        .expect("failed to create initial dispatcher");
    
    let ready_route = warp::path!("ready").and(warp::get()).and_then(ready);
    let download_dispatcher_route = warp::path("download_dispatcher")
        .and(warp::get())
        .and(warp::fs::file(format!("{}/target/debug/dispatcher-agent", ROOT.as_str())));
    let recv_trace_route = {
        let compiler = compiler.clone();
        warp::path!("recv_trace" / String)
            .and(warp::body::bytes())
            .and(warp::post())
            .and(warp::any().map(move || compiler.clone()))
            .and_then(recv_trace)
    };
    let is_compiling_route = {
        let compiler = compiler.clone();
        warp::path!("ok_if_not_compiling")
            .and(warp::get())
            .and(warp::any().map(move || compiler.clone()))
            .and_then(ok_if_not_compiling_handler)
    };
    let restart_dispatcher_route = {
        let compiler = compiler.clone();
        warp::path!("restart_dispatcher")
            .and(warp::post())
            .and(warp::any().map(move || compiler.clone()))
            .and_then(restart_dispatcher_handler)
    };
    let reset_dispatcher_route = {
        let compiler = compiler.clone();
        warp::path!("reset_dispatcher")
            .and(warp::post())
            .and(warp::any().map(move || compiler.clone()))
            .and_then(reset_dispatcher_handler)
    };

    let paths = ready_route
        .or(download_dispatcher_route)
        .or(recv_trace_route)
        .or(restart_dispatcher_route)
        .or(reset_dispatcher_route)
        .or(is_compiling_route);

    info!(target: "controller", "Controller listening");
    let (_addr, server) = warp::serve(paths).bind_with_graceful_shutdown(
        ([0, 0, 0, 0], 7999),
        suppress_and_log_err(handle_sigterm(compiler)),
    );
    server.await;
}
