use shared::function::*;

#[derive(Debug, Clone)]
pub struct Record {
    name: String,
    pub opts: FunctionOptions,
    pub contents: FunctionContents
}

impl Record {
    pub fn new(name: &str, opts: FunctionOptions, contents: FunctionContents) -> Record {
        Record {
            name: name.to_string(),
            opts: opts,
            contents: contents
        }
    }
}