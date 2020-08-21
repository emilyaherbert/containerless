use crate::dispatcher::function_table::FunctionTable;

use shared::response::*;

use std::sync::Arc;

pub async fn readiness_handler() -> Result<impl warp::Reply, warp::Rejection> {
    ok_response("To invoke: http://HOSTNAME/dispatcher/FUNCTION-NAME".to_string())
}

pub async fn dispatcher_handler(
    function_name: String, mut function_path: String, method: http::Method, body: bytes::Bytes,
    state: Arc<FunctionTable>,
) -> Result<impl warp::Reply, warp::Rejection> {
    debug!(target: "dispatcher", "received request for function {} with path {}", function_name, function_path);

    // Create the Function Manager for the function
    let fm_res = FunctionTable::get_function(&state, &function_name).await;
    if let Err(err) = fm_res {
        return Ok(hyper::Response::builder()
            .status(500)
            .body(hyper::Body::from(format!(
                "Error invoking function {}",
                err
            )))
            .unwrap());
    }
    let mut fm = fm_res.unwrap();

    // Invoke the function
    // If this is the first invocation, this will spin up a tracing instance
    debug!(target: "dispatcher", "invoking function {} with path {}", function_name, function_path);
    let body = hyper::Body::from(body);
    function_path = format!("/{}", function_path);
    return match fm.invoke(method, &function_path, body).await {
        Ok(resp) => Ok(resp),
        Err(err) => Ok(hyper::Response::builder()
            .status(500)
            .body(hyper::Body::from(format!(
                "Error invoking function {}",
                err
            )))
            .unwrap()),
    };
}

pub async fn compile_handler(
    function_name: String, state: Arc<FunctionTable>,
) -> Result<impl warp::Reply, warp::Rejection> {
    match FunctionTable::get_function(&state, &function_name).await {
        Ok(mut fm) => Ok(fm.extract_and_compile().await),
        Err(err) => Ok(hyper::Response::builder()
            .status(500)
            .body(hyper::Body::from(format!("{:?}", err)))
            .unwrap()),
    }
}

pub async fn get_mode_handler(
    function_name: String, state: Arc<FunctionTable>,
) -> Result<impl warp::Reply, warp::Rejection> {
    match FunctionTable::get_function(&state, &function_name).await {
        Ok(mut fm) => Ok(fm.get_mode().await),
        Err(err) => Ok(hyper::Response::builder()
            .status(500)
            .body(hyper::Body::from(format!("{:?}", err)))
            .unwrap()),
    }
}

pub async fn shutdown_function_instances_handler(
    function_name: String, state: Arc<FunctionTable>,
) -> Result<impl warp::Reply, warp::Rejection> {
    match FunctionTable::get_function(&state, &function_name).await {
        Ok(mut fm) => Ok(fm.shutdown().await),
        Err(err) => Ok(hyper::Response::builder()
            .status(500)
            .body(hyper::Body::from(format!("{:?}", err)))
            .unwrap()),
    }
}
