use crate::storage::SharedStorage;

use shared::file_contents::FileContents;

use hyper::Response;

pub async fn ping() -> Result<impl warp::Reply, warp::Rejection> {
    return Ok(Response::builder()
        .status(200)
        .body("Pinged function storage agent."));
}

pub async fn echo(message: String) -> Result<impl warp::Reply, warp::Rejection> {
    return Ok(Response::builder()
        .status(200)
        .body(message));
}

pub async fn get_function(path: String, storage: SharedStorage) -> Result<impl warp::Reply, warp::Rejection> {
    let mut storage = storage.lock().await;
    match storage.get(&path) {
        Err(err) => {
            error!("Error reading file {} : {:?} ", path, err);
            return Ok(Response::builder()
                .status(404)
                .body(format!("Could not read function {}.\n{:?}", path, err)));
        },
        Ok(file) => {
            return Ok(Response::builder().status(200).body(file.contents));
        }
    }
}

pub async fn create_function(path: String, contents: FileContents, storage: SharedStorage) -> Result<impl warp::Reply, warp::Rejection> {
    let mut storage = storage.lock().await;
    match storage.set(&path, &contents.contents) {
        Err(err) => {
            error!("Error creating file {} : {:?} ", path, err);
            return Ok(Response::builder()
                .status(404)
                .body(format!("Could not create function {}.\n{:?}", path, err)));
        },
        Ok(_file) => {
            return Ok(Response::builder().status(200).body(format!("{} created!", path)));
        }
    }
}

pub async fn delete_function(path: String, storage: SharedStorage) -> Result<impl warp::Reply, warp::Rejection> {
    let mut storage = storage.lock().await;
    match storage.remove(&path) {
        Err(err) => {
            error!("Error deleting file {}: {:?}", path, err);
            return Ok(Response::builder()
                .status(404)
                .body(format!("Could not delete function {}.\n{:?}", path, err)));
        },
        Ok(_file) => {
            return Ok(Response::builder().status(200).body(format!("{} deleted!", path)));
        }
    }
}

pub async fn list_functions(storage: SharedStorage) -> Result<impl warp::Reply, warp::Rejection> {
    let storage = storage.lock().await;
    let ordered_file_names = storage.get_all_keys();
    let body = ordered_file_names.join("\n");
    return Ok(Response::builder().status(200).body(body));
}