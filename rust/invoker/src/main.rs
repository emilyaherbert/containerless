mod container_pool;
mod error;
mod mpmc;
mod server;
mod types;

use futures::future::Future;

fn main() {
    println!("Starting Decontainerizer");

    hyper::rt::run(server::serve().map_err(|err| {
        println!("Error: {}", err);
        return ();
    }));
}
