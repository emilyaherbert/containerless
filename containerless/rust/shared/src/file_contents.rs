use serde::{Deserialize, Serialize};

#[derive(Deserialize, Serialize)]
pub struct FileContents {
    pub contents: String
}
