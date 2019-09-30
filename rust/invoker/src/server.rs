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
    let client1 = client.clone();

    let pool = Arc::new(ContainerPool::new(config, client));
    let pool2 = Arc::clone(&pool);

    let new_svc = move || {
        let pool2 = Arc::clone(&pool2);
        let client1 = client1.clone();
        service_fn(move |mut req| {
            if req.uri() == "/ping" {
                let new_uri = hyper::Uri::builder()
                .scheme("http")
                .authority("localhost:8081")
                .path_and_query(req.uri().path())
                .build()
                .unwrap();
                *req.uri_mut() = new_uri;
                future::Either::A(client1.request(req).from_err())
            } else {
                future::Either::B(ContainerPool::request(&pool2, req))
            }
        })
    };

    Server::bind(&addr).serve(new_svc).from_err()
}
