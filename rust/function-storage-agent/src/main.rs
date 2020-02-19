use warp::Filter;
use hyper::Response;
use tokio::fs::File;
use std::io;
use tokio::prelude::*;

async fn get_file(path: &str) -> Result<String, io::Error> {
    let mut file = File::open(format!("/storage/{}.js", &path)).await?;
    let mut buf = String::new();
    file.read_to_string(&mut buf).await?;
    return Ok(buf);
}

async fn get(path: String) -> Result<impl warp::Reply, warp::Rejection> {
    match get_file(&path).await {
        Err(err) => {
            eprintln!("Error reading file {}: {} ", path, err);
            return Ok(Response::builder().status(404).body("Could not read file".to_string()));
        },
        Ok(file) => {
            return Ok(Response::builder().status(200).body(file));
        }
    }
}

async fn ping() -> Result<impl warp::Reply, warp::Rejection> {
    return Ok(Response::builder().status(200).body("Function storage agent\n"));
}

#[tokio::main]
async fn main() {

    let get_route = warp::path!("get" / String)
        .and(warp::get())
        .and_then(get);

    let ping_route = warp::path!("ping")
        .and(warp::get())
        .and_then(ping);

    let paths = ping_route.or(get_route);

    warp::serve(paths).run(([0, 0, 0, 0], 8080)).await;
    println!("Function Runner Agent terminated");
}
