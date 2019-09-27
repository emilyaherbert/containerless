
use std::fs::File;
use std::io::prelude::*;
use hyper::{Body, Response, Server, Request, Method, StatusCode};
use hyper::rt::Future;
use hyper::service::service_fn;
use futures::future;
use futures::Stream;

// https://hyper.rs/

type BoxFut = Box<dyn Future<Item=Response<Body>, Error=hyper::Error> + Send>;

fn echo(req: Request<Body>) -> BoxFut {
    match (req.method(), req.uri().path()) {
        (&Method::GET, "/") => {
            return Box::new(future::ok(
                Response::new(Body::from("Try POSTing a file to /upload.\n"))
            ));
        },
        (&Method::POST, "/upload") => {
            println!("New file!");

            return Box::new(
                req.into_body()
                .concat2()
                .and_then(|body| {
                    File::create("file.txt")
                        .unwrap()
                        .write_all(&body)
                        .unwrap();
                    println!("File written to file.txt.");
                    future::ok(body)
                }).then(|_| {
                    Box::new(future::ok(
                        Response::new(Body::from("Done uploading!\n"))
                    ))
                })
            );
        },
        _ => {
            let mut response = Response::new(Body::empty());
            *response.status_mut() = StatusCode::NOT_FOUND;
            return Box::new(future::ok(response));
        },
    };
}

fn main() {
    let addr = ([127, 0, 0, 1], 3000).into();

    let server = Server::bind(&addr)
        .serve(|| service_fn(echo))
        .map_err(|e| eprintln!("server error: {}", e));

    hyper::rt::run(server);
}