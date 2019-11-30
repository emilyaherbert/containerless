//! Creates the server on which the invoker operates.
//! 
//! Incoming requests are sent to a `IsolationPool`.

use crate::{
    isolation_pool::IsolationPool,
    trace_runtime::Containerless,
    error::Error
};

use futures::{
    future::{self, Either},
    Future,
};
use hyper::service::service_fn;
use hyper::{Body, Client, Response, Server};
use shared::config::InvokerConfig;
use std::sync::Arc;

/// Starts a server that handles incoming requests.
/// 
/// 1. Creates a hyper `Client`.
/// 2. Creates an `IsolationPool` using
/// `config`, `containerless` if any, and the `Client`.
/// 3. Returns a server, where the service function sends incoming requests to
/// the `IsolationPool`.
/// 
/// The server intercepts messages with a `uri` of `/ping` for testing.
pub fn serve(
    config: Arc<InvokerConfig>,
    containerless: Option<Containerless>,
) -> impl future::Future<Item = (), Error = Error> {
    let addr = ([0, 0, 0, 0], config.bind_port).into();

    // TODO(arjun): The argumment 4 below is the number of threads. Why?
    let https = hyper_rustls::HttpsConnector::new(4);
    let client = Arc::new(Client::builder().build(https));

    let (pool, rx_shutdown) = IsolationPool::new(config.clone(), containerless, client);

    // TODO(emily): Put other special requests here: Get utilization. Shutdown containers?
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

    Server::bind(&addr)
        .serve(new_svc)
        .with_graceful_shutdown(rx_shutdown)
        .from_err()
}
