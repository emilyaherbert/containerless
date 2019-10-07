use super::trace_runtime::Decontainer;
use crate::config::Config;
use crate::container_pool::ContainerPool;
use crate::error::Error;
use futures::{
    future::{self, Either},
    Future,
};
use hyper::service::service_fn;
use hyper::{Body, Client, Response, Server};
use std::sync::Arc;

pub fn serve(config: Config) -> impl future::Future<Item = (), Error = Error> {
    let addr = ([0, 0, 0, 0], config.bind_port).into();

    let client = Arc::new(Client::new());
    let config = Arc::new(config);

    let pool = Arc::new(ContainerPool::new(config.clone(), client));
    let pool2 = Arc::clone(&pool);

    let new_svc = move || {
        let pool2 = Arc::clone(&pool2);
        let config = config.clone();
        service_fn(move |req| {
            if req.uri() == "/ping" {
                let resp = Response::new(Body::from("Hello from Rust (no container)"));
                Either::B(Either::A(future::ok(resp)))
            } else if let Some(containerless) = config.containerless {
                Either::A(Decontainer::new(containerless, req))
            } else {
                Either::B(Either::B(ContainerPool::request(&pool2, req)))
            }
        })
    };

    Server::bind(&addr).serve(new_svc).from_err()
}
