#[macro_use]
extern crate log;

mod error;
mod handlers;
mod routes;
mod storage;

use shared;
use storage::Storage;

// Based on:
// https://github.com/seanmonstar/warp/blob/master/examples/todos.rs

#[tokio::main]
async fn main() {
    shared::logger::init("http://controller-logger", 1);
    info!(target: "storage", "UP");
    let storage = Storage::new_shared_storage();
    shared::net::serve_until_sigterm(routes::routes(storage), 8080).await;
    info!(target: "storage", "DOWN");
}
