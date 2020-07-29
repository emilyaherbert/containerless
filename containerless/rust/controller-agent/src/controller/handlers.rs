use super::common::*;
use super::compiler::Compiler;
use super::error::Error;

use bytes;
use hyper::Response;

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

pub async fn create_function(name: String, body: bytes::Bytes) -> Result<impl warp::Reply, warp::Rejection> {
    let acceptable_chars: Vec<char> = "abcdefghijklmnopqrstuvwxyz1234567890.-".chars().collect();
    if !name.chars().all(|c| acceptable_chars.contains(&c)) {
        let err = Error::Parsing("Function names can only contain lower case alphanumeric characters, '.', and ','.".to_string());
        eprintln!("Error creating function {} : {:?} ", name, err);
        return Ok(Response::builder()
            .status(404)
            .body(format!("Could not create function: {:?}", err)));
    }
    info!("Adding function to storage...");
    match add_to_storage(&name, body).await {
        Err(err) => {
            eprintln!("Error creating function {} : {:?} ", name, err);
            return Ok(Response::builder()
                .status(404)
                .body(format!("Could not create function: {:?}", err)));
        },
        Ok(resp) => { 
            // TODO: test function here
            return Ok(Response::builder().status(200).body(resp));
        }
    }
}

async fn add_to_storage(name: &str, body: bytes::Bytes) -> Result<String, Error> {
    Ok(reqwest::Client::new()
        .post(&format!("http://localhost/storage/create_function/{}", name))
        .body(body)
        .send()
        .await?
        .text()
        .await?)
}