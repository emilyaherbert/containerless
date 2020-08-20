#[macro_use]
extern crate log;

mod handlers;
mod routes;
mod storage;

use storage::Storage;

// Based on:
// https://github.com/seanmonstar/warp/blob/master/examples/todos.rs

#[tokio::main]
async fn main() {
    let storage = Storage::new_shared_storage();
    warp::serve(routes::routes(storage))
        .run(([0, 0, 0, 0], 8080))
        .await;
    println!("Function Runner Agent terminated");
}
