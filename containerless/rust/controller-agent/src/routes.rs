use super::compiler::Compiler;
use super::handlers;

use std::sync::Arc;
use warp::Filter;

pub fn routes(
    compiler: Arc<Compiler>, root_str: &str
) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    ready_route()
        .or(download_dispatcher_route(root_str))
        .or(recv_trace_route(compiler.clone()))
        .or(is_compiling_route(compiler.clone()))
        .or(restart_dispatcher_route(compiler.clone()))
        .or(reset_dispatcher_route(compiler.clone()))
        .or(create_function_route())
        .or(delete_function_route())
        .or(shutdown_function_instances_route())
        .or(reset_function_route(compiler.clone()))
        .or(get_function_route())
        .or(list_functions_route())
}

fn ready_route() -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("ready")
        .and(warp::get())
        .and_then(handlers::ready)
}

fn download_dispatcher_route(root_str: &str) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path("download_dispatcher")
        .and(warp::get())
        .and(warp::fs::file(format!(
            "{}/target/debug/dispatcher-agent",
            root_str
        )))
}

fn recv_trace_route(compiler: Arc<Compiler>) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("recv_trace" / String)
        .and(warp::body::bytes())
        .and(warp::post())
        .and(with_compiler(compiler))
        .and_then(handlers::recv_trace)
}

fn is_compiling_route(compiler: Arc<Compiler>) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("ok_if_not_compiling")
        .and(warp::get())
        .and(with_compiler(compiler))
        .and_then(handlers::ok_if_not_compiling_handler)
}

fn restart_dispatcher_route(compiler: Arc<Compiler>) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("restart_dispatcher")
        .and(warp::post())
        .and(with_compiler(compiler))
        .and_then(handlers::restart_dispatcher_handler)
}

fn reset_dispatcher_route(compiler: Arc<Compiler>) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("reset_dispatcher")
        .and(warp::post())
        .and(with_compiler(compiler))
        .and_then(handlers::reset_dispatcher_handler)
}

fn create_function_route() -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("create_function" / String)
        .and(warp::post())
        .and(warp::body::bytes())
        .and_then(handlers::create_function)
}

fn delete_function_route() -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("delete_function" / String)
        .and(warp::get())
        .and_then(handlers::delete_function)
}

fn shutdown_function_instances_route() -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("shutdown_function_instances" / String)
        .and(warp::get())
        .and_then(handlers::shutdown_function_instances)
}

fn reset_function_route(compiler: Arc<Compiler>) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("reset_function" / String)
        .and(warp::get())
        .and(with_compiler(compiler))
        .and_then(handlers::reset_function)
}

fn get_function_route() -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("get_function" / String)
        .and(warp::get())
        .and_then(handlers::get_function)
}

fn list_functions_route() -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("list_functions")
        .and(warp::get())
        .and_then(handlers::list_functions)
}

fn with_compiler(compiler: Arc<Compiler>) -> impl Filter<Extract = (Arc<Compiler>,), Error = std::convert::Infallible> + Clone {
    warp::any().map(move || compiler.clone())
}