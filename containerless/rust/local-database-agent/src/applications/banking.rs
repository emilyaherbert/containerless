use serde::{Deserialize, Serialize};
use std::sync::atomic::{AtomicI64, Ordering};

#[derive(Serialize, Deserialize, Debug)]
struct AccountName {
    name: String
}

#[derive(Serialize, Deserialize, Debug)]
struct InternalAccount {
    name: String,
    balance: AtomicI64
}

#[derive(Serialize, Deserialize, Debug)]
struct Account {
    name: String,
    balance: f64
}

#[derive(Serialize, Deserialize, Debug)]
struct AccountTransaction {
    transaction: String,
    mutation: Account
}