mod storage;
mod error;

use storage::Storage;
use error::Error;

use hyper::Response;
use tokio::prelude::*;
use warp::Filter;
use std::sync::Arc;
use tokio::fs::File;
use bytes;

async fn get(path: String, storage: Arc<Storage>) -> Result<impl warp::Reply, warp::Rejection> {
    let mut storage = Arc::try_unwrap(storage).unwrap();
    match storage.get(&path) {
        Err(err) => {
            eprintln!("Error reading file {}: {:?} ", path, err);
            return Ok(Response::builder()
                .status(404)
                .body("Could not read file".to_string()));
        }
        Ok(file) => {
            // TODO(emily): Do something better than this.
            return Ok(Response::builder().status(200).body(String::from_utf8(file.contents.to_vec()).expect("oh no")));
        }
    }
}

async fn set(path: String, contents: bytes::Bytes, storage: Arc<Storage>) -> Result<impl warp::Reply, warp::Rejection> {
    let mut storage = Arc::try_unwrap(storage).unwrap();
    storage.set(&path, contents);
    return Ok(Response::builder().status(200).body("File stored!"));
}

async fn ping() -> Result<impl warp::Reply, warp::Rejection> {
    return Ok(Response::builder()
        .status(200)
        .body("Function storage agent\n"));
}

#[tokio::main]
async fn main() {
    let storage = Arc::new(storage::Storage::new());

    let get_route = {
        let storage = storage.clone();
        warp::path!("get" / String)
        .and(warp::get())
        .and(warp::any().map(move || storage.clone()))
        .and_then(get)
    };

    let set_route = {
        let storage = storage.clone();
        warp::path!("set" / String)
        .and(warp::body::bytes())
        .and(warp::get())
        .and(warp::any().map(move || storage.clone()))
        .and_then(set)
    };

    let ping_route = warp::path!("ping")
        .and(warp::get())
        .and_then(ping);

    let paths = ping_route
        .or(get_route)
        .or(set_route);
    warp::serve(paths).run(([0, 0, 0, 0], 8080)).await;
    println!("Function Runner Agent terminated");

}
