mod dispatcher;
mod error;
mod handlers;
mod routes;
pub mod trace_runtime;

use dispatcher::function_table::FunctionTable;
use dispatcher::{types, types::*};
use tokio::signal::unix::{signal, SignalKind};

#[macro_use]
extern crate log;

pub type Containerless = types::Containerless;

pub async fn main(decontainerized_functions: HashMap<&'static str, Containerless>) {
    shared::rsyslog::init_using_env();

    info!(target: "dispatcher", "Started dispatcher");
    let state = FunctionTable::new(decontainerized_functions).await;
    if let Err(err) = FunctionTable::adopt_running_functions(&state).await {
        error!(target: "dispatcher", "adopting functions: {}", err);
        return;
    }

    let routes = routes::routes(state.clone());

    info!(target: "dispatcher", "started listening");
    let (_addr, server) =
        warp::serve(routes).bind_with_graceful_shutdown(([0, 0, 0, 0], 8080), async {
            let mut sigterm = signal(SignalKind::terminate()).expect("registering SIGTERM handler");
            sigterm.recv().await;
            println!("Received SIGTERM");
        });
    server.await;
    FunctionTable::orphan(state).await;
    std::process::exit(0);
}
