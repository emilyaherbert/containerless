use super::Stack;

use shared::response;

use serde::{Deserialize, Serialize};
use rand::Rng;

#[derive(Serialize, Deserialize, Debug)]
pub struct Commit {
    pub sha: String
}

#[derive(Serialize, Deserialize)]
pub struct Status {
    sha: String,
    state: String
}

pub async fn status_get_handler(commits_stack: Stack<Commit>) -> Result<impl warp::Reply, warp::Rejection> {
    let commits_stack = commits_stack.lock().await;
    let mut rng = rand::thread_rng();

    match commits_stack.get(rng.gen_range(0, commits_stack.len())) {
        Some(commit) => {
            if commit.sha.len() > 1 {
                return response::ok_response(format!("{}", serde_json::to_string(&commit).expect("Could not create string from JSON.")));
            } else {
                return response::ok_response("{ \"body\": \"Sha not available.\" }".to_string());
            }
        },
        None => return response::ok_response("{ \"body\": \"No shas available.\" }".to_string())
    }
}

pub async fn status_post_handler(_status: Status) -> Result<impl warp::Reply, warp::Rejection> {
    // theoretically update status of internal commit DB
    return response::ok_response("{ \"body\": \"Done!\" }".to_string());
}