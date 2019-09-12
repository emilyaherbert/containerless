#[derive(Debug, Copy, Clone)]
pub enum Error {
    /** Equivalent to a runtime type-error in JavaScript */
    TypeError,
    /** Result of reaching an unknown portion of the trace. */
    Unknown,
}

impl std::fmt::Display for Error {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Error::TypeError => fmt.write_str("TypeError"),
            Error::Unknown => fmt.write_str("Unknown"),
        }
    }
}

impl std::error::Error for Error {}
