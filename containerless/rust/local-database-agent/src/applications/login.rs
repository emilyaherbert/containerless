use super::DB;

use shared::response;

use serde::{Deserialize, Serialize};
use serde_json;
use std::collections::HashMap;

#[derive(Serialize, Deserialize)]
pub struct User {
    pub username: String,
    pub password: String,
}

pub async fn login_handler(
    info: Option<HashMap<String, String>>, users_db: DB<User>,
) -> Result<impl warp::Reply, warp::Rejection> {
    let users_db = users_db.lock().await;
    if let None = info {
        return response::ok_response("{ \"body\": \"No query provided.\" }".to_string());
    }
    let info = info.unwrap();
    if !info.contains_key("username") {
        return response::ok_response("{ \"body\": \"No username in query.\" }".to_string());
    }
    let username = info.get("username").unwrap();
    match users_db.get(username) {
        Some(user) => {
            return response::ok_response(format!(
                "{}",
                serde_json::to_string(&user).unwrap_or("internal server error".to_string())
            ))
        }
        None => return response::ok_response("{ \"body\": \"User not found.\" }".to_string()),
    };
}
