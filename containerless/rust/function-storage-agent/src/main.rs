#[macro_use]
extern crate log;

mod error;
mod handlers;
mod routes;
mod storage;

use storage::Storage;
use shared;

// Based on:
// https://github.com/seanmonstar/warp/blob/master/examples/todos.rs

#[tokio::main]
async fn main() {
    let storage = Storage::new_shared_storage();
    shared::net::serve_until_sigterm(routes::routes(storage), 8080).await;
    println!("Function Runner Agent terminated");
}