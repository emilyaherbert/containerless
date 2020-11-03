use crate::error::Error;
use crate::record::Record;

use std::clone::Clone;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::Mutex;

pub type SharedStorage = Arc<Mutex<Storage>>;

#[derive(Debug)]
pub struct Storage {
    files: HashMap<String, Record>,
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

    pub fn get(&mut self, name: &str) -> Result<Record, Error> {
        match self.files.get(name) {
            Some(file) => Ok((*file).clone()),
            None => Err(Error::FileNotFound(format!("{} not found.", name))),
        }
    }

    pub fn get_all_keys(&self) -> Vec<String> {
        let mut keys: Vec<String> = self.files.keys().cloned().collect();
        keys.sort();
        keys
    }

    pub fn remove(&mut self, name: &str) -> Result<Record, Error> {
        match self.files.remove(name) {
            Some(file) => Ok(file),
            None => Err(Error::FileNotFound(format!("{} not found.", name))),
        }
    }

    pub fn set(&mut self, name: &str, record: Record) -> Result<String, Error> {
        if self.files.contains_key(name) {
            Err(Error::FileConflict(format!(
                "The name {} is already in use.",
                name
            )))
        } else {
            self.files.insert(name.to_string(), record);
            Ok(name.to_string())
        }
    }
}
