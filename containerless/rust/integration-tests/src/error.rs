use std::io;

#[derive(Debug)]
pub enum Error {
    IO(io::Error),
    Reqwest(reqwest::Error),
    Invoke(String),
    Compile(String),
    Timeout
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