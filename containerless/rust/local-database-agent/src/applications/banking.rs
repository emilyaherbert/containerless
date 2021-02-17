use super::DB;

use shared::response;

use serde::{Deserialize, Serialize};
use std::sync::atomic::{AtomicI64, Ordering};

#[derive(Serialize, Deserialize, Debug)]
pub struct AccountName {
    name: String,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct InternalAccount {
    pub name: String,
    pub balance: AtomicI64,
}

#[derive(Serialize, Deserialize, Debug)]
struct Account {
    name: String,
    balance: f64,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct AccountTransaction {
    transaction: String,
    mutation: Account,
}

pub async fn begin_handler() -> Result<impl warp::Reply, warp::Rejection> {
    response::ok_response("{ \"transaction\": \"135798642\" }".to_string())
}

pub async fn commit_handler(
    transaction: AccountTransaction, accounts_db: DB<InternalAccount>,
) -> Result<impl warp::Reply, warp::Rejection> {
    let accounts_db = accounts_db.lock().await;

    match accounts_db.get(&transaction.mutation.name) {
        Some(acc) => {
            let new_amount: i64 = (transaction.mutation.balance * 100.0) as i64;
            acc.balance.store(new_amount, Ordering::SeqCst);
            return response::ok_response("{ \"body\": \"Done!\" }".to_string());
        }
        None => return response::ok_response("{ \"body\": \"Account not found.\" }".to_string()),
    }
}

pub async fn balance_handler(
    account: AccountName, accounts_db: DB<InternalAccount>,
) -> Result<impl warp::Reply, warp::Rejection> {
    let accounts_db = accounts_db.lock().await;

    match accounts_db.get(&account.name) {
        Some(acc) => {
            return response::ok_response(format!(
                "{{ \"balance\": {} }}",
                (acc.balance.load(Ordering::SeqCst) / 100) as f64
            ))
        }
        None => return response::ok_response("{ \"body\": \"Account not found.\" }".to_string()),
    }
}
