use crate::error::Error;

use std::clone::Clone;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::Mutex;

#[derive(Debug, Clone)]
pub struct StorageFile {
    name: String,
    pub contents: String,
}

impl StorageFile {
    pub fn new(name: &str, contents: &str) -> StorageFile {
        StorageFile {
            name: name.to_string(),
            contents: contents.to_string(),
        }
    }
}

pub type SharedStorage = Arc<Mutex<Storage>>;

#[derive(Debug)]
pub struct Storage {
    files: HashMap<String, StorageFile>,
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

    pub fn get(&mut self, name: &str) -> Result<StorageFile, Error> {
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

    pub fn remove(&mut self, name: &str) -> Result<StorageFile, Error> {
        match self.files.remove(name) {
            Some(file) => Ok(file),
            None => Err(Error::FileNotFound(format!("{} not found.", name))),
        }
    }

    pub fn set(&mut self, name: &str, contents: &str) -> Result<String, Error> {
        if self.files.contains_key(name) {
            Err(Error::FileConflict(format!(
                "The name {} is already in use.",
                name
            )))
        } else {
            self.files
                .insert(name.to_string(), StorageFile::new(name, contents));
            Ok(name.to_string())
        }
    }
}
