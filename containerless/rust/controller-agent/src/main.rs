use warp::Filter;
#[macro_use]
extern crate lazy_static;

mod common;
mod compiler;
mod graceful_sigterm;
mod trace_compiler;
mod handlers;
mod routes;

use common::*;
use compiler::Compiler;
use graceful_sigterm::handle_sigterm;

#[tokio::main]
async fn main() {
    shared::rsyslog::init_using_env();

    let k8s_client = Arc::new(
        k8s::Client::from_kubeconfig_file(NAMESPACE)
            .await
            .expect("creating k8s::Client"),
    );

    let compiler = Compiler::new();
    assert!(
        compiler.cargo_build(None).await,
        "initial cargo build failed"
    );

    k8s_client
        .new_deployment(compiler::dispatcher_deployment_spec(0))
        .await
        .expect("failed to create initial dispatcher");

    info!(target: "controller", "Controller listening");
    let (_addr, server) = warp::serve(routes::routes(compiler.clone(), ROOT.as_str())).bind_with_graceful_shutdown(
        ([0, 0, 0, 0], 7999),
        suppress_and_log_err(handle_sigterm(compiler)),
    );
    server.await;
}
