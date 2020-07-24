use crate::error::Error;

use std::collections::HashMap;
use std::clone::Clone;
//use bytes;
use std::sync::Arc;
use tokio::sync::Mutex;

#[derive(Debug, Clone)]
pub struct StorageFile {
    name: String,
    //pub contents: bytes::Bytes
}

impl StorageFile {
    pub fn new(name: &str) -> StorageFile {
        StorageFile {
            name: name.to_string(),
            //contents: contents
        }
    }

    /*
    pub fn new(name: &str, contents: bytes::Bytes) -> StorageFile {
        StorageFile {
            name: name.to_string(),
            contents: contents
        }
    }
    */
}

pub type SharedStorage = Arc<Mutex<Storage>>;

#[derive(Debug)]
pub struct Storage {
    files: HashMap<String, StorageFile>
}

impl Storage {
    pub fn new() -> Storage {
        Storage {
            files: HashMap::new()
        }
    }

    pub fn new_shared_storage() -> SharedStorage {
        Arc::new(Mutex::new(Self::new()))
    }

    pub fn get(&mut self, name: &str) -> Result<StorageFile, Error> {
        match self.files.get(name) {
            Some(file) => Ok((*file).clone()),
            None => Err(Error::FileNotFound(format!("{} not found.", name)))
        }
    }

    pub fn get_all_keys(&self) -> Vec<String> {
        let mut keys: Vec<String> = self.files.keys().cloned().collect();
        keys.sort();
        keys
    }

    pub fn get_all(&self) -> Result<Vec<StorageFile>, Error> {
        let mut keys: Vec<String> = self.files.keys().cloned().collect();
        keys.sort();
        let mut ordered_files = Vec::new();
        for key in keys {
            let file = self.files.get(&key).unwrap();
            ordered_files.push(file.clone());
            /*
            self.files.get(&key)
                .and_then(|file| {
                    ordered_files.push(file.clone());
                    Some(file)
                })
                .ok_or(Error::FileNotFound(format!("{} not found.", name)));
            */
        }
        Ok(ordered_files)
    }

    // TODO: Need to test file before storing.
    pub fn set(&mut self, name: &str) {
        self.files.insert(name.to_string(), StorageFile::new(name));
    }
    /*
    pub fn set(&mut self, name: &str, contents: bytes::Bytes) {
        self.files.insert(name.to_string(), StorageFile::new(name, contents));
    }
    */
}