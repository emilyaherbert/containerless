use crate::storage::SharedStorage;

use shared::file_contents::FileContents;
use shared::response::*;

pub async fn ping() -> Result<impl warp::Reply, warp::Rejection> {
    return ok_response("Pinged function storage agent.".to_string());
}

pub async fn echo(message: String) -> Result<impl warp::Reply, warp::Rejection> {
    return ok_response(message);
}

pub async fn get_function(
    path: String, storage: SharedStorage,
) -> Result<impl warp::Reply, warp::Rejection> {
    let mut storage = storage.lock().await;
    match storage.get(&path) {
        Err(err) => {
            error!("Error reading file {} : {:?} ", path, err);
            error_response(format!("{:?}", err))
        }
        Ok(file) => ok_response(file.contents),
    }
}

pub async fn create_function(
    path: String, contents: FileContents, storage: SharedStorage,
) -> Result<impl warp::Reply, warp::Rejection> {
    let mut storage = storage.lock().await;
    match storage.set(&path, &contents.contents) {
        Err(err) => {
            error!("Error creating file {} : {:?} ", path, err);
            error_response(format!("{:?}", err))
        }
        Ok(_file) => ok_response(format!("{} created!", path)),
    }
}

pub async fn delete_function(
    path: String, storage: SharedStorage,
) -> Result<impl warp::Reply, warp::Rejection> {
    let mut storage = storage.lock().await;
    match storage.remove(&path) {
        Err(err) => {
            error!("Error deleting file {}: {:?}", path, err);
            error_response(format!("{:?}", err))
        }
        Ok(_file) => ok_response(format!("{} deleted!", path)),
    }
}

pub async fn list_functions(storage: SharedStorage) -> Result<impl warp::Reply, warp::Rejection> {
    let storage = storage.lock().await;
    let ordered_file_names = storage.get_all_keys();
    let body = ordered_file_names.join("\n");
    ok_response(body)
}
