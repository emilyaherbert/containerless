use serde_json::error::Error as JsonError;
use std::convert::From;

#[derive(Debug)]
pub enum Error {
    Json(JsonError),
}

impl std::fmt::Display for Error {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Error::Json(json_err) => {
                fmt.write_str("JSON error: ")?;
                return json_err.fmt(fmt);
            }
        }
    }
}

impl std::error::Error for Error {}

impl From<JsonError> for Error {
    fn from(e: JsonError) -> Self {
        return Error::Json(e);
    }
}
