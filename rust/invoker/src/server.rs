use shared::config::InvokerConfig;
use super::decontainer_pool::TracingPool;
use crate::error::Error;
use super::trace_runtime::Containerless;
use futures::{
    future::{self, Either},
    Future,
};
use hyper::service::service_fn;
use hyper::{Body, Client, Response, Server};
use std::sync::Arc;

pub fn serve(config: InvokerConfig, containerless: Option<Containerless>) -> impl future::Future<Item = (), Error = Error> {
    let addr = ([0, 0, 0, 0], config.bind_port).into();

    let client = Arc::new(Client::new());
    let config = Arc::new(config);

    let (pool, rx_shutdown) = TracingPool::new(config.clone(), containerless, client);

    let new_svc = move || {
        let pool = pool.clone();
        service_fn(move |req| {
            if req.uri() == "/ping" {
                let resp = Response::new(Body::from("Hello from Rust (no container)\n"));
                Either::A(future::ok(resp))
            } else {
                Either::B(pool.request(req))
            }
        })
    };

    Server::bind(&addr).serve(new_svc)
        .with_graceful_shutdown(rx_shutdown)
        .from_err()
}
