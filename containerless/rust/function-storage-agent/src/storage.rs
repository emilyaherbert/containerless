use crate::error::Error;

use std::collections::HashMap;

struct StorageFile {
    contents: String
}

pub struct Storage {
    files: HashMap<&'static str, StorageFile>
}

impl Storage {
    pub fn new() -> Storage {
        Storage {
            files: HashMap::new()
        }
    }
}