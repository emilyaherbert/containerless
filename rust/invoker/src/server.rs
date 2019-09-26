use crate::config::Config;
use crate::container_pool::ContainerPool;
use crate::error::Error;
use futures::{future, Future};
use hyper::service::service_fn;
use hyper::{Client, Server};
use std::sync::Arc;

pub fn serve(config: Config) -> impl future::Future<Item = (), Error = Error> {
    let addr = ([0, 0, 0, 0], config.bind_port).into();

    let client = Arc::new(Client::new());
    let config = Arc::new(config);

    let pool = Arc::new(ContainerPool::new(config, client));
    let pool2 = Arc::clone(&pool);

    let new_svc = move || {
        let pool2 = Arc::clone(&pool2);
        service_fn(move |req| {
            ContainerPool::request(&pool2, req).map(move |(resp, _duration)| resp)
        })
    };

    Server::bind(&addr).serve(new_svc).from_err()
}
