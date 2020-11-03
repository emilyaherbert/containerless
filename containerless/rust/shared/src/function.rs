use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct FunctionContents {
    pub contents: String,
    /// When set, all other functions get deleted
    pub exclusive: bool,
}

#[derive(Debug, Clone, Deserialize)]
pub struct FunctionOptions {
    pub containers_only: bool,
}

pub struct Function {
    pub opts: FunctionOptions,
    pub contents: FunctionContents,
}
