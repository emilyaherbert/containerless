use crate::storage::SharedStorage;
use crate::record::Record;

use shared::function::*;
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
            error_response(err.info())
        }
        Ok(file) => {
            let containerless_mode = if file.opts.containers_only {
                "disable-tracing"
            } else {
                "tracing"
            };
            ok_response_with_containerless_mode(file.contents.contents, containerless_mode.to_string())
        },
    }
}

pub async fn create_function(
    path: String, opts: FunctionOptions, contents: FunctionContents, storage: SharedStorage,
) -> Result<impl warp::Reply, warp::Rejection> {
    let mut storage = storage.lock().await;
    let record = Record::new(&path, opts, contents);
    match storage.set(&path, record) {
        Err(err) => {
            error!("Error creating file {} : {:?} ", path, err);
            error_response(err.info())
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
            error_response(err.info())
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
