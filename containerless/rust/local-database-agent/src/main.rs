#[macro_use]
extern crate log;

mod routes;
mod applications;
mod error;

use shared::logger;

#[tokio::main]
async fn main() {
    logger::init("http://controller-logger", 1);
    info!(target: "local-database", "UP");
    let routes = routes::routes();
    info!(target: "local-database", "LISTENING");
    shared::net::serve_until_sigterm(routes, 7998).await;
    info!(target: "local-database", "DOWN");
}
