use super::DB;

use shared::response;

use serde::{Deserialize, Serialize};
use serde_json;

#[derive(Serialize, Deserialize)]
pub struct User {
    pub username: String,
    pub password: String
}

pub async fn login_handler(info: User, users_db: DB<User>) -> Result<impl warp::Reply, warp::Rejection> {
    let users_db = users_db.lock().await;

    match users_db.get(&info.username) {
        Some(user) => return response::ok_response(format!("{}", serde_json::to_string(&user).unwrap_or("internal server error".to_string()))),
        None => return response::ok_response("{ \"body\": \"User not found.\" }".to_string())
    };
}