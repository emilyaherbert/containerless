use reqwest;
use std::io;

#[derive(Debug)]
pub enum Error {
    IO(io::Error),
    Reqwest(reqwest::Error),
    CompileError,
    Containerless(shared::containerless::error::Error)
}

impl std::fmt::Display for Error {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Error::IO(err) => err.fmt(fmt),
            Error::Reqwest(err) => err.fmt(fmt),
            Error::CompileError => fmt.write_str("CompileError"),
            Error::Containerless(err) => fmt.write_str(&err.info())
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

impl std::convert::From<shared::containerless::error::Error> for Error {
    fn from(error: shared::containerless::error::Error) -> Error {
        return Error::Containerless(error);
    }
}
