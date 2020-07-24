
use crate::handlers;
use crate::storage::SharedStorage;

use warp::Filter;
//use bytes;

pub fn routes(
    storage: SharedStorage,
) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    ping_route()
        .or(echo_route())
        .or(get_function_route(storage.clone()))
        .or(create_function_route(storage.clone()))
        .or(list_functions_route(storage.clone()))
}

fn ping_route() -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("ping")
        .and(warp::get())
        .and_then(handlers::ping)
}

fn echo_route() -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("echo" / String)
        .and(warp::get())
        .and_then(handlers::echo)
}

fn get_function_route(storage: SharedStorage) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("get-function" / String)
        .and(warp::get())
        .and(with_storage(storage))
        .and_then(handlers::get_function)
}

fn create_function_route(storage: SharedStorage) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("create-function" / String)
        .and(warp::get())
        .and(with_storage(storage))
        .and_then(handlers::create_function)
}

fn list_functions_route(storage: SharedStorage) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("list-functions")
        .and(warp::get())
        .and(with_storage(storage))
        .and_then(handlers::list_functions)
}

fn with_storage(storage: SharedStorage) -> impl Filter<Extract = (SharedStorage,), Error = std::convert::Infallible> + Clone {
    warp::any().map(move || storage.clone())
}