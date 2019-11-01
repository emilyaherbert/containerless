
use std::fs::File;
use std::io::prelude::*;
use hyper::{Body, Response, Server, Request, Method, StatusCode};
use hyper::rt::Future;
use hyper::service::service_fn;
use futures::future;
use futures::Stream;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Serialize, Deserialize)]
struct User {
    username: String,
    password: String
}

type BoxFut = Box<dyn Future<Item=Response<Body>, Error=hyper::Error> + Send>;

fn echo(req: Request<Body>) -> BoxFut {
    let mut users = HashMap::new();
    users.insert("javascript".to_string(), User { username: "javascript".to_string(), password: "rust".to_string() });
    users.insert("emily".to_string(), User { username: "emily".to_string(), password: "herbert".to_string() });

    let shas = vec![
        "1234567890",
        "qwerty"
    ];

    match (req.method(), req.uri().path()) {
        (&Method::GET, "/") => {
            return Box::new(future::ok(
                Response::new(Body::from("I don't understand this...\n"))
            ));
        },
        (&Method::POST, "/upload") => {
            return Box::new(
                req.into_body()
                .concat2()
                .and_then(|body| {
                    File::create("file.txt")
                        .unwrap()
                        .write_all(&body)
                        .unwrap();
                    future::ok(body)
                }).then(|_| {
                    Box::new(future::ok(
                        Response::new(Body::from(r#"{ "body": "Done uploading!\n" }"#))
                    ))
                })
            );
        },
        (&Method::POST, "/login") => {
            return Box::new(
                req.into_body()
                .concat2()
                .and_then(move |body| {
                    let b: User = serde_json::from_slice(&body).expect("Could not parse JSON body.");
                    let resp = match users.get(&b.username) {
                        Some(user) => Response::new(Body::from(format!("{}", serde_json::to_string(&user).expect("Could not create string from JSON.")))),
                        None => Response::new(Body::from("{ \"body\": \"User not found.\" }"))
                    };
                    Box::new(future::ok(resp))
                })
            );
        },
        (&Method::GET, "/status") => {
            let resp = match req.uri().query() {
                Some(q) => {
                    if q.len() == 0 {
                        Response::new(Body::from("No query provided.\n"))
                    } else {
                        match shas.first() {
                            Some(s) => Response::new(Body::from(format!("{}", s))),
                            None => Response::new(Body::from("No shas."))
                        }
                    }
                }
                None => Response::new(Body::from("No query provided.\n"))
            };
            return Box::new(future::ok(resp));
        },
        (&Method::POST, "/status") => {
            unimplemented!();
        },
        _ => {
            let mut response = Response::new(Body::empty());
            *response.status_mut() = StatusCode::NOT_FOUND;
            return Box::new(future::ok(response));
        },
    };
}

fn main() {
    let addr = ([127, 0, 0, 1], 7999).into();

    let server = Server::bind(&addr)
        .serve(|| service_fn(echo))
        .map_err(|e| eprintln!("server error: {}", e));

    hyper::rt::run(server);
}
