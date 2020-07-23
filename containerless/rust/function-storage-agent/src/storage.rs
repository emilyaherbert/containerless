use crate::error::Error;

use std::collections::HashMap;
use std::clone::Clone;
use bytes;

#[derive(Debug, Clone)]
pub struct StorageFile {
    name: String,
    pub contents: bytes::Bytes
}

impl StorageFile {
    pub fn new(name: &str, contents: bytes::Bytes) -> StorageFile {
        StorageFile {
            name: name.to_string(),
            contents: contents
        }
    }
}

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

    pub fn get(&mut self, name: &str) -> Result<StorageFile, Error> {
        match self.files.get(name) {
            Some(file) => Ok((*file).clone()),
            None => Err(Error::FileNotFound(format!("{} not found.", name)))
        }
    }

    // TODO: Need to test file before storing.
    pub fn set(&mut self, name: &str, contents: bytes::Bytes) {
        self.files.insert(name.to_string(), StorageFile::new(name, contents));
    }
}