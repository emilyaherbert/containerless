use std::io;
use std::string;

#[derive(Debug)]
pub enum Error {
    IO(io::Error),
    Parsing(String),
    Configuration(String),
    HTTP(reqwest::Error)
}

pub type CLIResult<T> = Result<T, Error>;

impl std::convert::From<io::Error> for Error {
    fn from(error: io::Error) -> Error {
        Error::IO(error)
    }
}

impl std::convert::From<string::FromUtf8Error> for Error {
    fn from(error: string::FromUtf8Error) -> Error {
        Error::Parsing(error.to_string())
    }
}

impl std::convert::From<std::env::VarError> for Error {
    fn from(error: std::env::VarError) -> Error {
        Error::Configuration(error.to_string())
    }
}

impl std::convert::From<reqwest::Error> for Error {
    fn from(error: reqwest::Error) -> Error {
        Error::HTTP(error)
    }
}