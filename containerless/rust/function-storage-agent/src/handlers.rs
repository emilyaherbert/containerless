use crate::storage::SharedStorage;

use hyper::Response;
//use bytes;

pub async fn ping() -> Result<impl warp::Reply, warp::Rejection> {
    return Ok(Response::builder()
        .status(200)
        .body("Function storage agent\n"));
}

pub async fn echo(message: String) -> Result<impl warp::Reply, warp::Rejection> {
    return Ok(Response::builder()
        .status(200)
        .body(message + "\n"));
}

pub async fn get(path: String, storage: SharedStorage) -> Result<impl warp::Reply, warp::Rejection> {
    let mut storage = storage.lock().await;
    match storage.get(&path) {
        Err(err) => {
            eprintln!("Error reading file {}: {:?} ", path, err);
            return Ok(Response::builder()
                .status(404)
                .body("Could not read file".to_string()));
        }
        Ok(file) => {
            // TODO(emily): Do something better than this.
            //return Ok(Response::builder().status(200).body(String::from_utf8(file.contents.to_vec()).expect("oh no")));
            return Ok(Response::builder().status(200).body(format!("{:?} contents", path)));
        }
    }
}

pub async fn set(path: String, storage: SharedStorage) -> Result<impl warp::Reply, warp::Rejection> {
    println!("Storage recieved path {:?}", path);
    let mut storage = storage.lock().await;
    storage.set(&path);
    return Ok(Response::builder().status(200).body("File stored!"));
}

/*
async fn set(path: String, contents: bytes::Bytes, storage: SharedStorage) -> Result<impl warp::Reply, warp::Rejection> {
    let mut storage = Arc::try_unwrap(storage).unwrap();
    storage.set(&path, contents);
    return Ok(Response::builder().status(200).body("File stored!"));
}
*/