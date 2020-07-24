mod storage;
mod routes;
mod handlers;
mod error;

use storage::Storage;

#[tokio::main]
async fn main() {
    let storage = Storage::new_shared_storage();
    warp::serve(routes::routes(storage)).run(([0, 0, 0, 0], 8080)).await;
    println!("Function Runner Agent terminated");
}
