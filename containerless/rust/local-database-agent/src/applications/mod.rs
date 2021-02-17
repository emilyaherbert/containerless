pub mod banking;
pub mod login;
pub mod status;
pub mod upload;

use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::Mutex;

pub type DB<T> = Arc<Mutex<HashMap<String, T>>>;
pub type Stack<T> = Arc<Mutex<Vec<T>>>;
