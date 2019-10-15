#[derive(Debug, Clone)]
pub enum Error {
    /** Equivalent to a runtime type-error in JavaScript */
    TypeError(String),
    /** Result of reaching an unknown portion of the trace. */
    Unknown,
}

impl std::fmt::Display for Error {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Error::TypeError(s) => {
                fmt.write_str("TypeError: ")?;
                fmt.write_str(s)
            }
            Error::Unknown => fmt.write_str("Unknown"),
        }
    }
}

impl std::error::Error for Error {}

pub fn type_error<T,S>(message: S) -> Result<T, Error>  where
  S : Into<String> {
    return Err(Error::TypeError(message.into()));
}
