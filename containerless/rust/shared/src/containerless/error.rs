use std::io;

#[derive(Debug, Display)]
pub enum Error {
    IO(io::Error),
    Reqwest(reqwest::Error),
    Invoke(String),
    Timeout,
    FromUtf8Error(std::string::FromUtf8Error),
    ParseIntError(std::num::ParseIntError),
    Storage(String),
    Dispatcher(String),
}

impl Error {
    /// This function makes printing slighlty prettier. Essentially it unwraps
    /// the core info about a message so that the internal error structure is
    /// not propogated and then shown to the user. It is not a super great
    /// system, and relies on the system hacker to understand what would qualify
    /// as a good error message and what wouldn't.
    pub fn info(&self) -> String {
        match self {
            Error::Storage(info) => info.to_owned(),
            Error::Dispatcher(info) => info.to_owned(),
            error => format!("internal containerless error: {:?}", error),
        }
    }
}

impl std::convert::From<io::Error> for Error {
    fn from(error: io::Error) -> Error {
        Error::IO(error)
    }
}

impl std::convert::From<reqwest::Error> for Error {
    fn from(error: reqwest::Error) -> Error {
        Error::Reqwest(error)
    }
}

impl std::convert::From<std::string::FromUtf8Error> for Error {
    fn from(error: std::string::FromUtf8Error) -> Error {
        Error::FromUtf8Error(error)
    }
}

impl std::convert::From<std::num::ParseIntError> for Error {
    fn from(error: std::num::ParseIntError) -> Error {
        Error::ParseIntError(error)
    }
}
