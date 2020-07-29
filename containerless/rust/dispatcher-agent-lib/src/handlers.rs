use crate::dispatcher::function_table::FunctionTable;

use std::sync::Arc;

pub async fn readiness_handler() -> Result<impl warp::Reply, warp::Rejection> {
    return Ok(hyper::Response::builder()
        .status(200)
        .body(hyper::Body::from(
            "To invoke: http://HOSTNAME/dispatcher/FUNCTION-NAME\n",
        ))
        .unwrap());
}

pub async fn dispatcher_handler(
    function_name: String, mut function_path: String, method: http::Method, body: bytes::Bytes,
    state: Arc<FunctionTable>,
) -> Result<impl warp::Reply, warp::Rejection> {
    debug!(target: "dispatcher", "received request for function {} with path {}", function_name, function_path);
    let mut fm = FunctionTable::get_function(&state, &function_name).await;
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
    let mut fm = FunctionTable::get_function(&state, &function_name).await;
    return Ok(fm.extract_and_compile().await);
}

pub async fn get_mode_handler(
    function_name: String, state: Arc<FunctionTable>,
) -> Result<impl warp::Reply, warp::Rejection> {
    let mut fm = FunctionTable::get_function(&state, &function_name).await;
    return Ok(fm.get_mode().await);
}