mod dispatcher;
mod error;
mod handlers;
mod routes;
pub mod trace_runtime;

use dispatcher::function_table::FunctionTable;
use dispatcher::{types, types::*};

#[macro_use]
extern crate log;

pub type Containerless = types::Containerless;

pub async fn main(decontainerized_functions: HashMap<&'static str, Containerless>) {
    env_logger::init();
    info!(target: "dispatcher", "UP");
    let state = FunctionTable::new(decontainerized_functions).await;
    if let Err(err) = FunctionTable::adopt_running_functions(&state).await {
        error!(target: "dispatcher", "adopting functions: {}", err);
        return;
    }

    let routes = routes::routes(state.clone());

    info!(target: "dispatcher", "LISTENING");
    shared::net::serve_until_sigterm(routes, 8080).await;
    //FunctionTable::orphan(state).await;
    info!(target: "dispatcher", "DOWN");
    std::process::exit(0);
}
