use super::compiler::CompilerHandle;
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

pub async fn handle_sigterm(mut compiler: CompilerHandle) -> Result<(), kube::Error> {
    let mut sigterm = signal(SignalKind::terminate()).expect("registering SIGTERM handler");
    sigterm.recv().await;
    info!("Received SIGTERM");

    compiler.shutdown().await;
    let k8s = k8s::Client::new(NAMESPACE).await?;
    future::try_join(delete_replica_sets(&k8s), delete_services(&k8s)).await?;
    return Ok(());
}
