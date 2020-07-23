mod storage;
mod error;

use storage::Storage;

use hyper::Response;
use std::io;
use tokio::prelude::*;
use warp::Filter;
use std::sync::Arc;
use tokio::fs::File;

/*
async fn get_file2(path: &str) -> Result<String, Error> {
    match self.files.get(path) {
        Some(file) => Ok(file.contents.clone()),
        None => Err(Error::FileNotFound(format!("{} not found.", path)))
    }
}
*/

async fn get_file(path: &str, storage: Arc<Storage>) -> Result<String, io::Error> {
    let mut file = File::open(format!("/storage/{}.js", &path)).await?;
    let mut buf = String::new();
    file.read_to_string(&mut buf).await?;
    return Ok(buf);
}

async fn get(path: String, storage: Arc<Storage>) -> Result<impl warp::Reply, warp::Rejection> {
    match get_file(&path, storage).await {
        Err(err) => {
            eprintln!("Error reading file {}: {} ", path, err);
            return Ok(Response::builder()
                .status(404)
                .body("Could not read file".to_string()));
        }
        Ok(file) => {
            return Ok(Response::builder().status(200).body(file));
        }
    }
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

    let ping_route = warp::path!("ping")
        .and(warp::get())
        .and_then(ping);

    let paths = ping_route.or(get_route);
    warp::serve(paths).run(([0, 0, 0, 0], 8080)).await;
    println!("Function Runner Agent terminated");

    /*
    let (_addr, server) = warp::serve(paths).bind_with_graceful_shutdown(
        ([0, 0, 0, 0], 7999),
        suppress_and_log_err(handle_sigterm(storage)),
    );
    server.await;
    */
}
