use crate::controller::common::*;
use super::compiler::Compiler;
use crate::controller::error::Error;

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

pub async fn delete_function(name: String) -> Result<impl warp::Reply, warp::Rejection> {
    // NOTE(emily): This is not good style, do something with combinging results or something
    match shutdown_function_instances_via_dispatcher(&name).await {
        Err(err) => {
            eprintln!("Error shutting down instances for function {} : {:?} ", name, err);
            Ok(Response::builder()
                .status(404)
                .body(format!("Could not shut down instances for function: {:?}", err)))
        },
        Ok(resp1) => {
            // TODO: delete decontainerized version
            match delete_from_storage(&name).await {
                Err(err) => {
                    eprintln!("Error deleting function {} : {:?} ", name, err);
                    return Ok(Response::builder()
                        .status(404)
                        .body(format!("Could not delete function: {:?}", err)));
                },
                Ok(resp2) => {
                    return Ok(Response::builder().status(200).body(resp1 + "\n" + &resp2));
                }
            }
        }
    }
}

pub async fn shutdown_function_instances(name: String) -> Result<impl warp::Reply, warp::Rejection> {
    match shutdown_function_instances_via_dispatcher(&name).await {
        Err(err) => {
            eprintln!("Error shutting down instances for function {} : {:?} ", name, err);
            Ok(Response::builder()
                .status(404)
                .body(format!("Could not shut down instances for function: {:?}", err)))
        },
        Ok(resp) => {
            Ok(Response::builder().status(200).body(resp))
        }
    }
}

pub async fn get_function(name: String) -> Result<impl warp::Reply, warp::Rejection> {
    match get_from_storage(&name).await {
        Err(err) => {
            eprintln!("Error describing function {} : {:?} ", name, err);
            return Ok(Response::builder()
                .status(404)
                .body(format!("Could not describe function: {:?}", err)));
        },
        Ok(resp) => {
            return Ok(Response::builder().status(200).body(resp));
        }
    }
}

pub async fn list_functions() -> Result<impl warp::Reply, warp::Rejection> {
    match list_from_storage().await {
        Err(err) => {
            eprintln!("Error listing functions: {:?} ", err);
            return Ok(Response::builder()
                .status(404)
                .body(format!("Could not list functions: {:?}", err)));
        },
        Ok(resp) => {
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

async fn delete_from_storage(name: &str) -> Result<String, Error> {
    Ok(reqwest::get(&format!("http://localhost/storage/delete_function/{}", name)).await?.text().await?)
}

async fn get_from_storage(name: &str) -> Result<String, Error> {
    Ok(reqwest::get(&format!("http://localhost/storage/get_function/{}", name)).await?.text().await?)
}

async fn list_from_storage() -> Result<String, Error> {
    Ok(reqwest::get("http://localhost/storage/list_functions").await?.text().await?)
}

async fn shutdown_function_instances_via_dispatcher(name: &str) -> Result<String, Error> {
    Ok(reqwest::get(&format!("http://localhost/dispatcher/shutdown_function_instances/{}", name)).await?.text().await?)
}