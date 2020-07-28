use super::common::*;
use super::compiler::Compiler;

use bytes;
use hyper::Response;
use serde::{Deserialize, Serialize};

#[derive(Deserialize, Serialize)]
pub struct FileContents {
    pub contents: String
}

pub async fn ready() -> Result<impl warp::Reply, warp::Rejection> {
    Ok(Response::builder().status(200).body("Controller agent\n"))
}

pub async fn recv_trace(
    name: String, trace: bytes::Bytes, compiler: Arc<Compiler>,
) -> Result<impl warp::Reply, warp::Rejection> {
    info!(target: "controller", "received trace for {} ({} bytes)", &name, trace.len());
    compiler.compile(name, trace);
    Ok(Response::builder().status(200).body(""))
}

pub async fn ok_if_not_compiling_handler(
    compiler: Arc<Compiler>,
) -> Result<impl warp::Reply, warp::Rejection> {
    Ok(compiler.ok_if_not_compiling())
}

pub async fn restart_dispatcher_handler(
    compiler: Arc<Compiler>,
) -> Result<impl warp::Reply, warp::Rejection> {
    Ok(compiler.recompile_dispatcher().await)
}

pub async fn reset_dispatcher_handler(
    compiler: Arc<Compiler>,
) -> Result<impl warp::Reply, warp::Rejection> {
    Ok(compiler.reset_dispatcher().await)
}

pub async fn create_function(name: String, body: bytes::Bytes, compiler: Arc<Compiler>) -> Result<impl warp::Reply, warp::Rejection> {
    Ok(compiler.reset_dispatcher().await)
}