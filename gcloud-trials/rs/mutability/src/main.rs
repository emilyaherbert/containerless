extern crate hyper;

mod runtime;

use hyper::rt::{self};
use hyper::{Body, Request, Response, Server};
use hyper::service::service_fn;
use futures::Future;
use runtime::*;

fn req_msft(ec: &mut ExecutionContext) {
    println!("Got Microsoft");
}

fn req_goog(ec: &mut ExecutionContext) {
    println!("Got Google");
}

fn serverless_main(ec: &mut ExecutionContext) {
    println!("Entered serverless_main");
    ec.request("http://www.microsoft.com", req_msft);
    ec.request("http://www.google.com", req_goog);
}

fn main() {
    let addr = ([127, 0, 0, 1], 3000).into();

    let server = Server::bind(&addr)
        .serve(|| {
            service_fn(move |_: Request<Body>| {
                Runtime::new(serverless_main).map(|_resp| {
                    Response::new(Body::from("hello, world"))
                })
                .map_err(|x| std::io::Error::last_os_error())
            })
        })
        .map_err(|e| eprintln!("server error: {}", e));

    rt::run(server);
}

