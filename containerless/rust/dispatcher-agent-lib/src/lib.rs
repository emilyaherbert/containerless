mod error;
pub mod trace_runtime;
mod dispatcher;

use dispatcher::function_table::FunctionTable;
use tokio::signal::unix::{signal, SignalKind};
use dispatcher::{types, types::*};
use warp::Filter;

#[macro_use]
extern crate log;

pub type Containerless = types::Containerless;

async fn readiness_handler() -> Result<impl warp::Reply, warp::Rejection> {
    return Ok(hyper::Response::builder()
        .status(200)
        .body(hyper::Body::from(
            "To invoke: http://HOSTNAME/dispatcher/FUNCTION-NAME\n",
        ))
        .unwrap());
}

async fn dispatcher_handler(
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

async fn compile_handler(
    function_name: String, state: Arc<FunctionTable>,
) -> Result<impl warp::Reply, warp::Rejection> {
    let mut fm = FunctionTable::get_function(&state, &function_name).await;
    return Ok(fm.extract_and_compile().await);
}

async fn get_mode_handler(
    function_name: String, state: Arc<FunctionTable>,
) -> Result<impl warp::Reply, warp::Rejection> {
    let mut fm = FunctionTable::get_function(&state, &function_name).await;
    return Ok(fm.get_mode().await);
}

pub async fn main(decontainerized_functions: HashMap<&'static str, Containerless>) {
    shared::rsyslog::init_using_env();

    info!(target: "dispatcher", "Started dispatcher");
    let state = FunctionTable::new(decontainerized_functions).await;
    if let Err(err) = FunctionTable::adopt_running_functions(&state).await {
        error!(target: "dispatcher", "adopting functions: {}", err);
        return;
    }

    let extract_state = {
        let state = state.clone();
        warp::any().map(move || state.clone())
    };

    let readiness_route = warp::get()
        .and(warp::path!("readinessProbe"))
        .and_then(readiness_handler);

    let dispatcher_route = warp::path!(String / String)
        .and(warp::method())
        .and(warp::filters::body::bytes())
        .and(extract_state.clone())
        .and_then(dispatcher_handler);

    let extract_and_compile_route = warp::path!("compile" / String)
        .and(warp::post())
        .and(extract_state.clone())
        .and_then(compile_handler);

    let get_mode_route = warp::path!("mode" / String)
        .and(warp::get())
        .and(extract_state.clone())
        .and_then(get_mode_handler);

    let paths = readiness_route
        .or(extract_and_compile_route)
        .or(get_mode_route)
        .or(dispatcher_route);

    info!(target: "dispatcher", "started listening");
    let (_addr, server) =
        warp::serve(paths).bind_with_graceful_shutdown(([0, 0, 0, 0], 8080), async {
            let mut sigterm = signal(SignalKind::terminate()).expect("registering SIGTERM handler");
            sigterm.recv().await;
            println!("Received SIGTERM");
        });
    server.await;
    FunctionTable::orphan(state).await;
    std::process::exit(0);
}
