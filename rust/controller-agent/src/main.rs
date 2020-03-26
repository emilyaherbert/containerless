use hyper::Response;
use log::{error, info};
use std::future::Future;
use warp::Filter;

mod graceful_sigterm;

use graceful_sigterm::handle_sigterm;

async fn ready() -> Result<impl warp::Reply, warp::Rejection> {
    return Ok(Response::builder()
        .status(200)
        .body("Function storage agent\n"));
}

async fn suppress_and_log_err<F, E>(f: F)
where
    F: Future<Output = Result<(), E>>,
    E: std::error::Error,
{
    if let Err(err) = f.await {
        error!("Error: {:?}", err);
    }
}

#[tokio::main]
async fn main() {
    env_logger::init();
    info!("Started Controller");

    let ready_route = warp::path!("ready").and(warp::get()).and_then(ready);
    let download_dispatcher_route = warp::path("download_dispatcher")
        .and(warp::get())
        .and(warp::fs::file("/dispatcher-agent/target/debug/dispatcher-agent"));

    let paths = ready_route.or(download_dispatcher_route);

    let (_addr, server) = warp::serve(paths)
        .bind_with_graceful_shutdown(([0, 0, 0, 0], 80), suppress_and_log_err(handle_sigterm()));
    server.await;
}
