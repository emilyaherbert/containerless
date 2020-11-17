pub mod banking;
pub mod upload;
pub mod login;
pub mod status;

use std::sync::Arc;
use tokio::sync::Mutex;
use std::collections::HashMap;

pub type DB<T> = Arc<Mutex<HashMap<String, T>>>;
pub type Stack<T> = Arc<Mutex<Vec<T>>>;