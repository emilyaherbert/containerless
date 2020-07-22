use super::compiler::Compiler;
use crate::common::*;
use k8s;
use kube;
use lazy_static::lazy_static;
use regex::Regex;
use tokio::signal::unix::{signal, SignalKind};

fn is_function(name: &String) -> bool {
    lazy_static! {
        static ref RE: Regex = Regex::new("^function-(.*)$").unwrap();
    }
    return RE.is_match(name);
}

fn is_tracing_function(name: &String) -> bool {
    lazy_static! {
        static ref RE: Regex = Regex::new("^function-tracing-(.*)$").unwrap();
    }
    return RE.is_match(name);
}

async fn delete_service(k8s: &k8s::Client, name: String) -> Result<(), kube::Error> {
    info!(target: "controller", "Deleting service/{}", &name);
    return k8s.delete_service(name.as_str()).await;
}

async fn delete_replica_set(k8s: &k8s::Client, name: String) -> Result<(), kube::Error> {
    info!(target: "controller", "Deleting replicaset/{}", &name);
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

async fn delete_tracing_pods(k8s: &k8s::Client) -> Result<(), kube::Error> {
    let pods = k8s
        .list_pods()
        .await?
        .into_iter()
        .map(|(name, _)| name)
        .filter(is_tracing_function)
        .collect::<Vec<_>>();
    let deleters = pods.iter().map(|name| k8s.delete_pod(&name));
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

pub async fn delete_dynamic_resources(
    k8s_client: &k8s::Client, delete_dispatcher: bool,
) -> Result<(), kube::Error> {
    future::try_join4(
        delete_replica_sets(&k8s_client),
        delete_services(&k8s_client),
        if delete_dispatcher {
            future::Either::Left(k8s_client.delete_deployment("dispatcher"))
        } else {
            future::Either::Right(future::ok(()))
        },
        delete_tracing_pods(&k8s_client),
    )
    .await?;
    return Ok(());
}

pub async fn handle_sigterm(compiler: Arc<Compiler>) -> Result<(), kube::Error> {
    let mut sigterm = signal(SignalKind::terminate()).expect("registering SIGTERM handler");
    sigterm.recv().await;
    info!("Received SIGTERM");
    compiler.shutdown().await;
    return Ok(());
}
