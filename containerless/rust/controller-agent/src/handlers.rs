use super::compiler::Compiler;
use crate::controller::common::*;

use shared::error::{*, constructors::*};
use shared::file_contents::FileContents;
use shared::response::*;

use bytes;
use std::env;
use std::io::prelude::*;
use std::io::BufReader;
use std::process::{Command, Stdio};

pub async fn ready() -> Result<impl warp::Reply, warp::Rejection> {
    ok_response("Controller agent".to_string())
}

pub async fn system_ready() -> Result<impl warp::Reply, warp::Rejection> {
    ok_response("Controller agent".to_string())
}

pub async fn recv_trace(
    name: String, trace: bytes::Bytes, compiler: Arc<Compiler>,
) -> Result<impl warp::Reply, warp::Rejection> {
    info!(target: "controller", "received trace for {} ({} bytes)", &name, trace.len());
    compiler.compile(name, trace);
    ok_response("".to_string())
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

pub async fn create_function(
    name: String, contents: FileContents, compiler: Arc<Compiler>,
) -> Result<std::result::Result<http::response::Response<std::string::String>, http::Error>, warp::Rejection> {
    let acceptable_chars: Vec<char> = "abcdefghijklmnopqrstuvwxyz1234567890.-".chars().collect();
    if !name.chars().all(|c| acceptable_chars.contains(&c)) {
        let err = parsing_error("Function names can only contain lower case alphanumeric characters, '.', and ','.");
        error!(target: "controller", "Error creating function {} : {:?} ", name, err);
        return error_response(err);
    }
    if let Err(err) = check_function_compatibility(&contents.contents) {
        error!(target: "controller", "Error checking function compatibility {} to storage : {:?} ", name, err);
        return error_response(err);
    }
    if let Err(err) = add_to_storage(&name, contents).await {
        error!(target: "controller", "Error adding function {} to storage : {:?} ", name, err);
        return error_response(err);
    }
    if let Err(err) = add_to_compiler(&name, compiler.clone()).await {
        error!(target: "controller", "Error adding function {} to compiler : {:?} ", name, err);
        if let Err(err) = delete_from_storage(&name).await {
            error!(target: "controller", "Error deleting function {} : {:?} ", name, err);
        }
        return error_response(err);
    }
    return ok_response(format!("Function {} successfully created!", name));
}

pub async fn delete_function(
    name: String, compiler: Arc<Compiler>,
) -> Result<std::result::Result<http::response::Response<std::string::String>, http::Error>, warp::Rejection> {
    if let Err(err) = reset_function_via_compiler(&name, compiler).await {
        error!(target: "controller", "Error reseting function {} : {:?} ", name, err);
        return error_response(err);
    }
    if let Err(err) = delete_from_storage(&name).await {
        error!(target: "controller", "Error deleting function {} : {:?} ", name, err);
        return error_response(err);
    }
    if let Err(err) = shutdown_function_instances_via_dispatcher(&name).await {
        error!(target: "controller", "Error shutting down instances for function {} : {:?} ", name, err);
        return error_response(err);
    }
    return ok_response(format!("Function {} successfully deleted!", name));
}

pub async fn shutdown_function_instances(
    name: String,
) -> Result<impl warp::Reply, warp::Rejection> {
    match shutdown_function_instances_via_dispatcher(&name).await {
        Err(err) => {
            error!(target: "controller", "Error shutting down instances for function {} : {:?} ", name, err);
            error_response(err)
        }
        Ok(resp) => ok_response(resp)
    }
}

pub async fn reset_function(
    name: String, compiler: Arc<Compiler>,
) -> Result<impl warp::Reply, warp::Rejection> {
    match reset_function_via_compiler(&name, compiler).await {
        Err(err) => {
            error!("Error reseting function {} : {:?} ", name, err);
            error_response(err)
        }
        Ok(resp) => ok_response(resp)
    }
}

pub async fn get_function(name: String) -> Result<impl warp::Reply, warp::Rejection> {
    match get_from_storage(&name).await {
        Err(err) => {
            error!("Error describing function {} : {:?} ", name, err);
            error_response(err)
        }
        Ok(resp) => ok_response(resp)
    }
}

pub async fn list_functions() -> Result<impl warp::Reply, warp::Rejection> {
    match list_from_storage().await {
        Err(err) => {
            error!("Error listing functions: {:?} ", err);
            error_response(err)
        }
        Ok(resp) => ok_response(resp)
    }
}

async fn add_to_storage(name: &str, contents: FileContents) -> Result<String, Error> {
    let resp = reqwest::Client::new()
        .post(&format!(
            "http://localhost/storage/create_function/{}",
            name
        ))
        .json(&contents)
        .send()
        .await?;
    filter_errors_from_responses(resp.status(), resp.text().await?)
}

async fn add_to_compiler(name: &str, compiler: Arc<Compiler>) -> Result<String, Error> {
    let resp_status = compiler.create_function(&name).await;
    filter_errors_from_responses(resp_status, format!("Function {} created!", name))
}

async fn delete_from_storage(name: &str) -> Result<String, Error> {
    let resp = reqwest::get(&format!(
            "http://localhost/storage/delete_function/{}",
            name
        ))
        .await?;
    filter_errors_from_responses(resp.status(), resp.text().await?)
}

async fn get_from_storage(name: &str) -> Result<String, Error> {
    let resp = reqwest::get(&format!("http://localhost/storage/get_function/{}", name)).await?;
    filter_errors_from_responses(resp.status(), resp.text().await?)
}

async fn list_from_storage() -> Result<String, Error> {
    let resp = reqwest::get("http://localhost/storage/list_functions").await?;
    filter_errors_from_responses(resp.status(), resp.text().await?)
}

async fn shutdown_function_instances_via_dispatcher(name: &str) -> Result<String, Error> {
    let resp = reqwest::get(&format!(
            "http://localhost/dispatcher/shutdown_function_instances/{}",
            name
        ))
        .await?;
    filter_errors_from_responses(resp.status(), resp.text().await?)
}

async fn reset_function_via_compiler(name: &str, compiler: Arc<Compiler>) -> Result<String, Error> {
    let resp_status = compiler.reset_function(&name).await;
    filter_errors_from_responses(resp_status, format!("Trace could not be reset for function {}!",name))
}

fn check_function_compatibility(code: &str) -> Result<String, Error> {
    let mut path_vec: Vec<&str> = env!("CARGO_MANIFEST_DIR").split("/").collect();
    path_vec.truncate(path_vec.len() - 2);
    path_vec.push("javascript");
    path_vec.push("js-transform");
    path_vec.push("dist");
    path_vec.push("index.js");
    let path = path_vec.join("/");

    let mut transform = Command::new("node")
        .arg(path)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()?;

    transform
        .stdin
        .as_mut()
        .expect("opening stdin")
        .write_all(code.as_bytes())?;

    let exit = transform.wait()?;

    if exit.success() {
        let out = if let Some(ref mut stdout) = transform.stdout {
            let mut br = BufReader::new(stdout);
            let mut s = String::new();
            br.read_to_string(&mut s)?;
            Ok(s)
        } else {
            Ok("".to_string())
        };
        return out;
    } else {
        let err = if let Some(ref mut stderr) = transform.stderr {
            let mut br = BufReader::new(stderr);
            let mut s = String::new();
            br.read_to_string(&mut s)?;
            Err(parsing_error(&s))
        } else {
            Err(parsing_error("error"))
        };
        return err;
    }
}
