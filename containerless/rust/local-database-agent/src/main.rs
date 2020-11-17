#[macro_use]
extern crate log;

mod routes;
mod applications;
mod error;

use applications::{DB, Stack, login, status, banking};
use shared::logger;

use std::sync::Arc;
use tokio::sync::Mutex;
use std::collections::HashMap;
use std::sync::atomic::AtomicI64;

#[tokio::main]
async fn main() {
    logger::init("http://controller-logger", 1);
    info!(target: "local-database", "UP");

    let mut users = HashMap::new();
    users.insert("javascript".to_string(), login::User { username: "javascript".to_string(), password: "rust".to_string() });
    users.insert("emily".to_string(), login::User { username: "emily".to_string(), password: "herbert".to_string() });
    let users_db: DB<login::User> = Arc::new(Mutex::new(users));

    let commits = vec![
        status::Commit { sha: "1234567890".to_string() },
        status::Commit { sha: "qwerty".to_string() },
        status::Commit { sha: "z".to_string() },
    ];
    let commits_stack: Stack<status::Commit> = Arc::new(Mutex::new(commits));
    
    let mut accounts = HashMap::new();
    accounts.insert("coffee".to_string(), banking::InternalAccount { name: "coffee".to_string(), balance: AtomicI64::new(10000) });
    accounts.insert("tea".to_string(), banking::InternalAccount { name: "tea".to_string(), balance: AtomicI64::new(1234500) });
    accounts.insert("milk".to_string(), banking::InternalAccount { name: "milk".to_string(), balance: AtomicI64::new(66600) });
    let accounts_db: DB<banking::InternalAccount> = Arc::new(Mutex::new(accounts));

    let routes = routes::routes(users_db, commits_stack, accounts_db);

    info!(target: "local-database", "LISTENING");
    shared::net::serve_until_sigterm(routes, 7998).await;
    info!(target: "local-database", "DOWN");
}
