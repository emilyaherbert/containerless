extern crate lazy_static;

mod controller;
mod handlers;
mod routes;
mod trace_compiler;

use controller::compiler;
use shared::common::*;
use shared::logger;

#[tokio::main]
async fn main() {
    logger::init(1);

    info!(target: "controller", "UP");

    let k8s_client = Arc::new(
        k8s::Client::from_kubeconfig_file(NAMESPACE)
            .await
            .expect("creating k8s::Client"),
    );

    let compiler = compiler::Compiler::new();
    assert!(
        compiler.cargo_build(None).await,
        "initial cargo build failed"
    );

    k8s_client
        .new_deployment(compiler::dispatcher_deployment_spec(0))
        .await
        .expect("failed to create initial dispatcher");

    info!(target: "controller", "LISTENING");

    shared::net::serve_until_sigterm(routes::routes(compiler.clone(), ROOT.as_str()), 7999).await;
    compiler.shutdown().await;
    info!(target: "controller", "DOWN");
}
