use std::io;

#[derive(Debug)]
pub enum Error {
    IO(io::Error),
    Reqwest(reqwest::Error),
    Invoke(String),
    Timeout,
    FromUtf8Error(std::string::FromUtf8Error),
    ParseIntError(std::num::ParseIntError)
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