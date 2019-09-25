use hyper::{Body, Response, Server, Request, Method, StatusCode};
use hyper::rt::Future;
use hyper::service::service_fn;
use futures::future;

// https://hyper.rs/

type BoxFut = Box<dyn Future<Item=Response<Body>, Error=hyper::Error> + Send>;

fn echo(req: Request<Body>) -> BoxFut {
    let mut response = Response::new(Body::empty());

    match (req.method(), req.uri().path()) {
        (&Method::GET, "/") => {
            *response.body_mut() = Body::from("Try POSTing a file to /upload.\n");
        },
        (&Method::POST, "/upload") => {
            println!("New file!");
            *response.body_mut() = Body::from("Done uploading!\n");
        },
        _ => {
            *response.status_mut() = StatusCode::NOT_FOUND;
        },
    };

    Box::new(future::ok(response))
}

fn main() {
    let addr = ([127, 0, 0, 1], 3000).into();

    let server = Server::bind(&addr)
        .serve(|| service_fn(echo))
        .map_err(|e| eprintln!("server error: {}", e));

    hyper::rt::run(server);
}