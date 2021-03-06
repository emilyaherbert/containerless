use super::compiler::Compiler;
use crate::controller::error::Error;

use shared::common::*;
use shared::containerless::dispatcher;
use shared::containerless::storage;
use shared::function::Function;
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
    name: String, func: Function, compiler: Arc<Compiler>,
) -> Result<
    std::result::Result<http::response::Response<std::string::String>, http::Error>,
    warp::Rejection,
> {
    info!(target: "controller", "CREATE_FUNCTION {}: entered handler", name);

    let exclusive = func.exclusive;

    // Check that the function name is ok
    let acceptable_chars: Vec<char> = "abcdefghijklmnopqrstuvwxyz1234567890.-".chars().collect();
    if !name.chars().all(|c| acceptable_chars.contains(&c)) {
        let err = Error::Parsing(
            "Function names can only contain lower case alphanumeric characters, '.', and ','."
                .to_string(),
        );
        error!(target: "controller", "CREATE_FUNCTION {} : Error {:?} ", name, err);
        return error_response(err.info());
    }

    // Check that the body of the function is compatibile with instrumentation
    info!(target: "controller", "CREATE_FUNCTION {}: checking function compatibility", name);
    if let Err(err) = check_function_compatibility(&func.contents) {
        error!(target: "controller", "CREATE_FUNCTION {} : Error {:?} ", name, err);
        return error_response(err.info());
    }

    // Add the function to storage
    info!(target: "controller", "CREATE_FUNCTION {}: adding to storage", name);
    if let Err(err) = storage::add(&name, func).await {
        error!(target: "controller", "CREATE_FUNCTION {} : Error adding to storage {:?} ", name, err);
        return error_response(err.info());
    }

    // Add the function to the compiler
    info!(target: "controller", "CREATE_FUNCTION {}: adding to compiler", name);
    if let Err(err) = add_to_compiler(&name, compiler.clone(), exclusive).await {
        error!(target: "controller", "CREATE_FUNCTION {} : Error adding to compiler {:?} ", name, err);
        if let Err(err) = storage::delete(&name).await {
            error!(target: "controller", "CREATE_FUNCTION {} : Error deleting from storage {:?} ", name, err);
        }
        return error_response(err.info());
    }

    // Done!
    info!(target: "controller", "CREATE_FUNCTION {}: successful!", name);
    return ok_response(format!("Function {} successfully created!", name));
}

pub async fn delete_function(
    name: String, compiler: Arc<Compiler>,
) -> Result<
    std::result::Result<http::response::Response<std::string::String>, http::Error>,
    warp::Rejection,
> {
    info!(target: "controller", "DELETE_FUNCTION {}: entered handler", name);

    // Remove all of the pods for the function
    info!(target: "controller", "DELETE_FUNCTION {}: shutting down instances via dispatcher", name);
    let res1 = dispatcher::shutdown_function_instances(&name).await;

    // Remove the compiled trace for a function
    info!(target: "controller", "DELETE_FUNCTION {}: removing trace via compiler", name);
    let res2 = reset_function_via_compiler(&name, compiler).await;

    // Delete the function from storage
    info!(target: "controller", "DELETE_FUNCTION {}: deleting from storage", name);
    let res3 = storage::delete(&name).await;

    // Done!
    if let Err(err) = res1 {
        error!(target: "controller", "DELETE_FUNCTION {}: Error shutting down instances via dispatcher {:?}", name, err);
        return error_response(err.info());
    }
    if let Err(err) = res2 {
        error!(target: "controller", "DELETE_FUNCTION {}: Error removnig trace via compiler {:?}", name, err);
        return error_response(err.info());
    }
    if let Err(err) = res3 {
        error!(target: "controller", "DELETE_FUNCTION {}: Error deleting from storage {:?}", name, err);
        return error_response(err.info());
    }
    return ok_response(format!("Function {} successfully deleted!", name));
}

pub async fn shutdown_function_instances(
    name: String,
) -> Result<impl warp::Reply, warp::Rejection> {
    match dispatcher::shutdown_function_instances(&name).await {
        Err(err) => {
            error!(target:"controller", "SHUTDOWN_FUNCTION_INSTANCES: Error {:?} ", err);
            error_response(err.info())
        }
        Ok(resp) => ok_response(resp),
    }
}

pub async fn dispatcher_version_handler(
    compiler: Arc<Compiler>,
) -> Result<impl warp::Reply, warp::Rejection> {
    let version = compiler.dispatcher_version().await;
    ok_response(version.to_string())
}

pub async fn reset_function(
    name: String, compiler: Arc<Compiler>,
) -> Result<impl warp::Reply, warp::Rejection> {
    match reset_function_via_compiler(&name, compiler).await {
        Err(err) => {
            error!(target:"controller", "RESET_FUNCTION: Error {:?} ", err);
            error_response(err.info())
        }
        Ok(resp) => ok_response(resp),
    }
}

pub async fn get_function(name: String) -> Result<impl warp::Reply, warp::Rejection> {
    match storage::get(&name).await {
        Err(err) => {
            error!(target:"controller", "GET_FUNCTION: Error {:?} ", err);
            error_response(err.info())
        }
        Ok(resp) => ok_response(resp),
    }
}

pub async fn list_functions() -> Result<impl warp::Reply, warp::Rejection> {
    match storage::list().await {
        Err(err) => {
            error!(target:"controller", "LIST_FUNCTIONS: Error {:?} ", err);
            error_response(err.info())
        }
        Ok(resp) => ok_response(resp),
    }
}

async fn add_to_compiler(
    name: &str, compiler: Arc<Compiler>, exclusive: bool,
) -> Result<String, Error> {
    let resp_status = compiler.create_function(&name, exclusive).await;
    response_into_result(resp_status.as_u16(), format!("Function {} created!", name))
        .map_err(Error::Compiler)
}

async fn reset_function_via_compiler(name: &str, compiler: Arc<Compiler>) -> Result<String, Error> {
    let resp_status = compiler.reset_function(&name).await;
    match resp_status.as_u16() {
        200 => Ok(format!("Trace reset for function {}!", name)),
        other => Err(Error::Compiler(format!(
            "Trace could not be reset for function {}. (ERRCODE {:?})",
            name, other
        ))),
    }
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
            Err(Error::Parsing(s))
        } else {
            Err(Error::Parsing("error".to_string()))
        };
        return err;
    }
}
