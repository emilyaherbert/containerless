
use crate::applications::upload;

use shared::response;

use warp::Filter;

pub fn routes() -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    ready_route()
        .or(upload_route())
}

fn ready_route() -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("ready")
        .and(warp::get())
        .and_then(ready_handler)
}

fn upload_route() -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("upload")
        .and(warp::body::json())
        .and(warp::post())
        .and_then(upload::upload_handler)
}

pub async fn ready_handler() -> Result<impl warp::Reply, warp::Rejection> {
    response::ok_response("local database ready".to_string())
}