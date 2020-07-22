use thiserror::Error;
#[derive(Debug, Error)]
pub enum Error {
    /** Equivalent to a runtime type-error in JavaScript */
    #[error("Error::TypeError({0})")]
    TypeError(String),
    /** Result of reaching an unknown portion of the trace. */
    #[error("Error::Unknown")]
    Unknown,
    /** Run out of gas. */
    #[error("Error::OutOfGas")]
    OutOfGas,
    #[error("Error::Json({0})")]
    Json(#[from] serde_json::Error),
    #[error("Error::String({0})")]
    String(#[from] std::str::Utf8Error),
}

pub fn type_error<T>(message: impl Into<String>) -> Result<T, Error> {
    return Err(Error::TypeError(message.into()));
}

pub fn not_a_function<T>(fun: impl Into<String>) -> Result<T, Error> {
    return Err(Error::TypeError(format!(
        "{} is not a function.",
        fun.into()
    )));
}
