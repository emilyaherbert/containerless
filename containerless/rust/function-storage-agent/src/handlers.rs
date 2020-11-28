use crate::storage::SharedStorage;

use shared::function::Function;
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
            error!("Error reading func {} : {:?} ", path, err);
            error_response(err.info())
        }
        Ok(func) => {
            let mode = if func.containers_only {
                "disable-tracing"
            } else {
                "tracing"
            };
            ok_response_with_containerless_mode(func.contents, mode.to_string())
        }
    }
}

pub async fn create_function(
    path: String, func: Function, storage: SharedStorage,
) -> Result<impl warp::Reply, warp::Rejection> {
    println!(
        "storage create function with containers_only: {}",
        func.containers_only
    );
    let mut storage = storage.lock().await;
    match storage.set(&path, func.clone()) {
        Err(err) => {
            error!("Error creating func {} : {:?} ", path, err);
            error_response(err.info())
        }
        Ok(_) => ok_response("added to storage".to_string()),
    }
}

pub async fn delete_function(
    path: String, storage: SharedStorage,
) -> Result<impl warp::Reply, warp::Rejection> {
    let mut storage = storage.lock().await;
    match storage.remove(&path) {
        Err(err) => {
            error!("Error deleting func {}: {:?}", path, err);
            error_response(err.info())
        }
        Ok(_func) => ok_response(format!("{} deleted!", path)),
    }
}

pub async fn list_functions(storage: SharedStorage) -> Result<impl warp::Reply, warp::Rejection> {
    let storage = storage.lock().await;
    let ordered_func_names = storage.get_all_keys();
    let body = ordered_func_names.join("\n");
    ok_response(body)
}
