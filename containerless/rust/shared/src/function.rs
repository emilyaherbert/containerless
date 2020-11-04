use serde::{Deserialize, Serialize};

#[derive(Deserialize, Serialize, Debug, Clone)]
pub struct Function {
    pub contents: String,
    /// When set, all other functions get deleted
    pub exclusive: bool,
    pub containers_only: bool
}
