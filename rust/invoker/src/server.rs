use crate::config::Config;
use hyper::rt::Future;
use hyper::service::service_fn;
use hyper::{Client, Server};
use std::sync::Arc;

pub fn serve(config: Config) -> impl Future<Item = (), Error = crate::error::Error> {
    let addr = ([0, 0, 0, 0], config.bind_port).into();

    let client = Arc::new(Client::new());
    let config = Arc::new(config);

    let pool = Arc::new(crate::container_pool::ContainerPool::new(config, client));
    let pool2 = Arc::clone(&pool);

    let new_svc = move || {
        let pool2 = Arc::clone(&pool2);
        service_fn(move |req| pool2.request(req))
    };
    pool.create_containers(5)
        .and_then(move |()| Server::bind(&addr).serve(new_svc).from_err())
}
