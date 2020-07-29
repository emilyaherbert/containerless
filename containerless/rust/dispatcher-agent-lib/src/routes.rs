use crate::handlers;
use crate::dispatcher::function_table::FunctionTable;

use std::sync::Arc;
use warp::Filter;

pub fn routes(state: Arc<FunctionTable>) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    readiness_route()
        .or(dispatcher_route(state.clone()))
        .or(extract_and_compile_route(state.clone()))
        .or(get_mode_route(state.clone()))
}

fn readiness_route() -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::get()
        .and(warp::path!("readinessProbe"))
        .and_then(handlers::readiness_handler)
}

fn dispatcher_route(state: Arc<FunctionTable>) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!(String / String)
        .and(warp::method())
        .and(warp::filters::body::bytes())
        .and(with_state(state))
        .and_then(handlers::dispatcher_handler)
}

fn extract_and_compile_route(state: Arc<FunctionTable>) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("compile" / String)
        .and(warp::post())
        .and(with_state(state))
        .and_then(handlers::compile_handler)
}

fn get_mode_route(state: Arc<FunctionTable>) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("mode" / String)
        .and(warp::get())
        .and(with_state(state))
        .and_then(handlers::get_mode_handler)
}

fn with_state(state: Arc<FunctionTable>) -> impl Filter<Extract = (Arc<FunctionTable>,), Error = std::convert::Infallible> + Clone {
    warp::any().map(move || state.clone())
}