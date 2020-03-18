use futures::future;
use hyper::Response;
use k8s;
use kube;
use lazy_static::lazy_static;
use log::{error, info};
use regex::Regex;
use std::future::Future;
use tokio::signal::unix::{signal, SignalKind};
use warp::Filter;

static NAMESPACE: &'static str = "containerless";

async fn ready() -> Result<impl warp::Reply, warp::Rejection> {
    return Ok(Response::builder()
        .status(200)
        .body("Function storage agent\n"));
}

fn is_function(name: &String) -> bool {
    lazy_static! {
        static ref RE: Regex = Regex::new("^function-(.*)$").unwrap();
    }
    return RE.is_match(name);
}

async fn delete_service(k8s: &k8s::Client, name: String) -> Result<(), kube::Error> {
    info!("Deleting service/{}", &name);
    return k8s.delete_service(name.as_str()).await;
}

async fn delete_replica_set(k8s: &k8s::Client, name: String) -> Result<(), kube::Error> {
    info!("Deleting replicaset/{}", &name);
    return k8s.delete_replica_set(name.as_str()).await;
}

async fn delete_services(k8s: &k8s::Client) -> Result<(), kube::Error> {
    let services = k8s.list_services().await?;
    let deleters = services
        .into_iter()
        .map(|(name, _)| name)
        .filter(is_function)
        .map(|name| delete_service(&k8s, name));
    future::try_join_all(deleters).await?;
    return Ok(());
}

async fn delete_replica_sets(k8s: &k8s::Client) -> Result<(), kube::Error> {
    let replica_sets = k8s.list_services().await?;
    let deleters = replica_sets
        .into_iter()
        .map(|(name, _)| name)
        .filter(is_function)
        .map(|name| delete_replica_set(&k8s, name));
    future::try_join_all(deleters).await?;
    return Ok(());
}

async fn handle_sigterm() -> Result<(), kube::Error> {
    info!("Received SIGTERM");
    let mut sigterm = signal(SignalKind::terminate()).expect("registering SIGTERM handler");
    sigterm.recv().await;

    let k8s = k8s::Client::new(NAMESPACE).await?;

    future::try_join(
        delete_replica_sets(&k8s),
        delete_services(&k8s),
    )
    .await?;
    return Ok(());
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

    let ready_route = warp::path!("ready").and(warp::get()).and_then(ready);

    let paths = ready_route;

    let (_addr, server) = warp::serve(paths)
        .bind_with_graceful_shutdown(([0, 0, 0, 0], 80), suppress_and_log_err(handle_sigterm()));
    server.await;
}
