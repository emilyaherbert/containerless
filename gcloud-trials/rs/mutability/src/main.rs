#![allow(unused_assignments, unused_imports, unused_variables)]
extern crate hyper;

pub mod runtime;

use hyper::rt::{self};
use hyper::{Body, Request, Response, Server};
use hyper::service::{service_fn, service_fn_ok};
use futures::Future;
use runtime::*;
use std::sync::Arc;
#[macro_use]
use futures::*;
use bumpalo::Bump;
use std::cell::RefCell;

// task_local!(static ARENA: std::cell::RefCell<i32> = RefCell::new(3434));

fn req_goog(ec: &mut ExecutionContext) {
    println!("Got Google");
}

fn req_msft(ec: &mut ExecutionContext) {
    println!("Got Microsoft");
}


// fn serverless_main<'a>(arena: &'a bumpalo::Bump, ec: &mut ExecutionContext) {
//     println!("Entered serverless_main");

//     let cell = arena.alloc(100);
//     *cell = 300;
//     // let arena2 = Arc::clone(&arena);
//     let cell2 = arena.alloc(200);
//     ec.request("http://www.microsoft.com",  Box::new(move |ec| {
//         let _ = arena.alloc(2);
//         *cell2 = 400;
//         println!("Got Microsoft");
//     }));
//     // ec.request("http://www.google.com", req_goog);
// }

fn main() {

    let addr = ([127, 0, 0, 1], 3000).into();

    let server = Server::bind(&addr)
        .serve(|| {
            service_fn(move |req: Request<Body>| {

                let task: Box<Decontainer<ExampleTask>> = Box::new(Decontainer::new());
                task.map(|_resp| {
                    Response::new(Body::from("hello, world"))
                })
                .map_err(|x| std::io::Error::last_os_error())
            })
        })
        .map_err(|e| eprintln!("server error: {}", e));

    rt::run(server);
}

