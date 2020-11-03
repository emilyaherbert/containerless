use crate::error::Error;

use shared::function::Function;

use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::Mutex;

pub type SharedStorage = Arc<Mutex<Storage>>;

#[derive(Debug)]
pub struct Storage {
    files: HashMap<String, Function>,
}

impl Storage {
    pub fn new() -> Storage {
        Storage {
            files: HashMap::new(),
        }
    }

    pub fn new_shared_storage() -> SharedStorage {
        Arc::new(Mutex::new(Self::new()))
    }

    pub fn get(&mut self, name: &str) -> Result<Function, Error> {
        match self.files.get(name) {
            Some(func) => Ok((*func).clone()),
            None => Err(Error::NotFound(format!("{} not found.", name))),
        }
    }

    pub fn get_all_keys(&self) -> Vec<String> {
        let mut keys: Vec<String> = self.files.keys().cloned().collect();
        keys.sort();
        keys
    }

    pub fn remove(&mut self, name: &str) -> Result<Function, Error> {
        match self.files.remove(name) {
            Some(func) => Ok(func),
            None => Err(Error::NotFound(format!("{} not found.", name))),
        }
    }

    pub fn set(&mut self, name: &str, func: Function) -> Result<String, Error> {
        if self.files.contains_key(name) {
            Err(Error::Conflict(format!(
                "The name {} is already in use.",
                name
            )))
        } else {
            self.files.insert(name.to_string(), func);
            Ok(name.to_string())
        }
    }
}
