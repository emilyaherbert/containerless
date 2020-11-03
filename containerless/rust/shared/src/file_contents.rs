use serde::{Deserialize, Serialize};

#[derive(Deserialize, Serialize)]
pub struct FileContents {
    pub contents: String,
    /// When set, all other functions get deleted
    pub exclusive: bool,
}
