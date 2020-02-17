use reqwest;
use std::io;

#[derive(Debug)]
pub enum Error {
    IO(io::Error),
    Reqwest(reqwest::Error),
    CompileError,
}

impl std::fmt::Display for Error {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Error::IO(err) => err.fmt(fmt),
            Error::Reqwest(err) => err.fmt(fmt),
            Error::CompileError => fmt.write_str("CompileError"),
        }
    }
}

impl std::error::Error for Error {}

impl std::convert::From<io::Error> for Error {
    fn from(error: io::Error) -> Error {
        return Error::IO(error);
    }
}

impl std::convert::From<reqwest::Error> for Error {
    fn from(error: reqwest::Error) -> Error {
        return Error::Reqwest(error);
    }
}
