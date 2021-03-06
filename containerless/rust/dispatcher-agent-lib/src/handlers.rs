use crate::dispatcher::function_table::FunctionTable;

use shared::response::*;

use std::sync::Arc;

pub async fn readiness_handler() -> Result<impl warp::Reply, warp::Rejection> {
    ok_response("To invoke: http://HOSTNAME/dispatcher/FUNCTION-NAME".to_string())
}

pub async fn dispatcher_handler(
    function_name: String, mut function_path: String, function_query: Option<String>,
    method: http::Method, body: bytes::Bytes, state: Arc<FunctionTable>,
) -> Result<impl warp::Reply, warp::Rejection> {
    function_path = match function_query {
        Some(query) => format!("/{}?{}", function_path, query),
        None => format!("/{}", function_path),
    };
    debug!(target: "dispatcher", "INVOKE {}: recieved request with path_and_query {}", function_name, function_path);

    // Create the Function Manager for the function
    let fm_res = FunctionTable::get_function(&state, &function_name).await;
    if let Err(err) = fm_res {
        return Ok(hyper::Response::builder()
            .status(500)
            .body(hyper::Body::from(err.info()))
            .unwrap());
    }
    let mut fm = fm_res.unwrap();

    // Invoke the function
    // If this is the first invocation, this will spin up a tracing instance
    debug!(target: "dispatcher", "INVOKE {}: invoking with path_and_query {}", function_name, function_path);
    let body = hyper::Body::from(body);
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

pub async fn dispatcher_handler2(
    function_name: String, function_query: Option<String>, method: http::Method,
    body: bytes::Bytes, state: Arc<FunctionTable>,
) -> Result<impl warp::Reply, warp::Rejection> {
    dispatcher_handler(
        function_name,
        "".to_string(),
        function_query,
        method,
        body,
        state,
    )
    .await
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
    if !FunctionTable::function_manager_exists(&state, &function_name).await {
        return Ok(hyper::Response::builder()
            .status(200)
            .body(hyper::Body::from(format!(
                "No pods to shut down for function {}",
                function_name
            )))
            .unwrap());
    }

    match FunctionTable::get_function(&state, &function_name).await {
        Ok(mut fm) => Ok(fm.shutdown().await),
        // NOTE(emily): The error case should not happen, as we checked just
        // above that the function manager exists for this function.
        Err(err) => Ok(hyper::Response::builder()
            .status(500)
            .body(hyper::Body::from(format!("{:?}", err)))
            .unwrap()),
    }
}
