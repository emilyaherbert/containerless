use reqwest;
use std::io;

#[derive(Debug)]
pub enum Error {
    IO(io::Error),
    Reqwest(reqwest::Error),
    CompileError,
    FileNotFound,
}

impl std::fmt::Display for Error {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Error::IO(err) => err.fmt(fmt),
            Error::Reqwest(err) => err.fmt(fmt),
            Error::CompileError => fmt.write_str("CompileError"),
            Error::FileNotFound => fmt.write_str("FileNotFound"),
        }
    }
}

impl std::error::Error for Error {}

impl std::convert::From<io::Error> for Error {
    fn from(error: io::Error) -> Error {
        return Error::IO(error);
    }
}

impl std::convert::From<(reqwest::Error, usize)> for Error {
    fn from(error: (reqwest::Error, usize)) -> Error {
        return Error::Reqwest(error.0);
    }
}

impl std::convert::From<reqwest::Error> for Error {
    fn from(error: reqwest::Error) -> Error {
        return Error::Reqwest(error);
    }
}
