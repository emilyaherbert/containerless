use serde::Deserialize;
use serde_json::Value as JsonValue;

#[derive(Debug, Deserialize)]
pub struct Request {
    pub path: String,
    pub query: JsonValue,
    pub body: JsonValue,
}

impl Request {
    pub fn from_string_vec(s: &str) -> Vec<Self> {
        serde_json::from_str(s).unwrap_or_else(|e| {
            panic!(
                "Error deserializing Vec<Request>. Error: {:?}. JSON string: {:?}",
                e, &s
            )
        })
    }
}
