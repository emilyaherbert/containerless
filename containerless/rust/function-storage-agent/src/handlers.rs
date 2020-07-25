use crate::storage::SharedStorage;

use hyper::Response;
//use bytes;

pub async fn ping() -> Result<impl warp::Reply, warp::Rejection> {
    return Ok(Response::builder()
        .status(200)
        .body("Pinged function storage agent.\n"));
}

pub async fn echo(message: String) -> Result<impl warp::Reply, warp::Rejection> {
    return Ok(Response::builder()
        .status(200)
        .body(message + "\n"));
}

pub async fn get_function(path: String, storage: SharedStorage) -> Result<impl warp::Reply, warp::Rejection> {
    let mut storage = storage.lock().await;
    match storage.get(&path) {
        Err(err) => {
            eprintln!("Error reading file {} : {:?} ", path, err);
            return Ok(Response::builder()
                .status(404)
                .body(format!("Could not read function {}.\n{:?}", path, err)));
        },
        Ok(_file) => {
            // TODO(emily): Do something better than this.
            //return Ok(Response::builder().status(200).body(String::from_utf8(file.contents.to_vec()).expect("oh no")));
            return Ok(Response::builder().status(200).body(format!("{:?} contents\n", path)));
        }
    }
}

pub async fn create_function(path: String, storage: SharedStorage) -> Result<impl warp::Reply, warp::Rejection> {
    let mut storage = storage.lock().await;
    match storage.set(&path) {
        Err(err) => {
            eprintln!("Error creating file {} : {:?} ", path, err);
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
            eprintln!("Error deleting file {}: {:?}", path, err);
            return Ok(Response::builder()
                .status(404)
                .body(format!("Could not delete function {}.\n{:?}", path, err)));
        },
        Ok(_file) => {
            return Ok(Response::builder().status(200).body(format!("{} deleted!", path)));
        }
    }
}

/*
async fn set(path: String, contents: bytes::Bytes, storage: SharedStorage) -> Result<impl warp::Reply, warp::Rejection> {
    let mut storage = Arc::try_unwrap(storage).unwrap();
    storage.set(&path, contents);
    return Ok(Response::builder().status(200).body("File stored!"));
}
*/

pub async fn list_functions(storage: SharedStorage) -> Result<impl warp::Reply, warp::Rejection> {
    let storage = storage.lock().await;
    let ordered_file_names = storage.get_all_keys();
    let body = ordered_file_names.join("\n");
    return Ok(Response::builder().status(200).body(body));
}