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
    function_name: String, mut function_path: String, function_query: String, method: http::Method, body: bytes::Bytes,
    state: Arc<FunctionTable>,
) -> Result<impl warp::Reply, warp::Rejection> {
    debug!(target: "dispatcher", "received request for function {} with path {} and query {}", function_name, function_path, function_query);
    let mut fm = FunctionTable::get_function(&state, &function_name).await;
    debug!(target: "dispatcher", "invoking function {} with path {} and query {}", function_name, function_path, function_query);
    let body = hyper::Body::from(body);
    function_path = format!("/{}?{}", function_path, function_query);
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

pub async fn shutdown_function_instances_handler(
    function_name: String, state: Arc<FunctionTable>,
) -> Result<impl warp::Reply, warp::Rejection> {
    let mut fm = FunctionTable::get_function(&state, &function_name).await;
    return Ok(fm.shutdown().await);
}

pub async fn system_status_handler(state: Arc<FunctionTable>,) -> Result<impl warp::Reply, warp::Rejection> {
    match FunctionTable::system_status(&state).await {
        Err(err) => {
            error!(target: "dispatcher", "Error getting system status : {:?} ", err);
            Ok(hyper::Response::builder()
                .status(500)
                .body(format!("Could not get system status: {:?}", err)))
        },
        Ok(resp) => {
            Ok(hyper::Response::builder()
            .status(200)
            .body(format!("{}", resp)))
        }
    }
}

pub async fn system_status_ok_handler(state: Arc<FunctionTable>,) -> Result<impl warp::Reply, warp::Rejection> {
    match FunctionTable::system_status_ok(&state).await {
        Err(err) => {
            error!(target: "dispatcher", "Error getting system status : {:?} ", err);
            Ok(hyper::Response::builder()
                .status(500)
                .body(format!("Could not get system status: {:?}", err)))
        },
        Ok(resp) => {
            if !resp {
                error!(target: "dispatcher", "System in broken state.");
                Ok(hyper::Response::builder()
                    .status(500)
                    .body("System in broken state.".to_string()))
            } else {
                Ok(hyper::Response::builder()
                    .status(200)
                    .body("System is okay.".to_string()))
            }
        }
    }
}