
use crate::applications::{DB, Stack, upload, login, status, banking};

use shared::response;

use warp::Filter;
use std::collections::HashMap;

pub fn routes(users_db: DB<login::User>, commits_stack: Stack<status::Commit>, accounts_db: DB<banking::InternalAccount>) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    ready_route()
        .or(upload_route())
        .or(login_route(users_db.clone()))
        .or(status_get_route(commits_stack.clone()))
        .or(status_post_route())
        .or(begin_route())
        .or(commit_route(accounts_db.clone()))
        .or(balance_route(accounts_db.clone()))
}

fn ready_route() -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("ready")
        .and(warp::get())
        .and_then(ready_handler)
}

fn upload_route() -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("upload")
        .and(warp::post())
        .and(warp::body::json())
        .and_then(upload::upload_handler)
}

fn login_route(users_db: DB<login::User>) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("login")
        .and(warp::get())
        .and(
            warp::query::query()
                .map(Some)
                .or_else(|_| async { Ok::<(Option<HashMap<String, String>>,), std::convert::Infallible>((None,)) }),
        )
        .and(with_shared(users_db))
        .and_then(login::login_handler)
}

fn status_get_route(commits_stack: Stack<status::Commit>) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("status")
        .and(warp::get())
        .and(with_shared(commits_stack))
        .and_then(status::status_get_handler)
}

fn status_post_route() -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("status")
        .and(warp::post())
        .and(warp::body::json())
        .and_then(status::status_post_handler)
}

fn begin_route() -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("begin")
        .and(warp::get())
        .and_then(banking::begin_handler)
}

fn commit_route(accounts_db: DB<banking::InternalAccount>) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("commit")
        .and(warp::post())
        .and(warp::body::json())
        .and(with_shared(accounts_db))
        .and_then(banking::commit_handler)
}

fn balance_route(accounts_db: DB<banking::InternalAccount>) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("balance")
        .and(warp::post())
        .and(warp::body::json())
        .and(with_shared(accounts_db))
        .and_then(banking::balance_handler)
}

fn with_shared<T>(db: T) -> impl Filter<Extract = (T,), Error = std::convert::Infallible> + Clone
    where T: std::marker::Send + std::clone::Clone {
    warp::any().map(move || db.clone())
}

pub async fn ready_handler() -> Result<impl warp::Reply, warp::Rejection> {
    response::ok_response("local database ready".to_string())
}
