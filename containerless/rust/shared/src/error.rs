use std::io;

#[derive(Debug)]
pub enum UserError {
    FileNotFound(String),
    FileConflict(String),
    Parsing(String)
}

#[derive(Debug)]
pub enum SystemError {
    IO(String),
    HTTP(String),
    Containerless(String),
}

#[derive(Debug)]
pub enum Error {
    User(UserError),
    System(SystemError)
}

impl std::convert::From<io::Error> for Error {
    fn from(error: io::Error) -> Error {
        Error::System(SystemError::IO(error.to_string()))
    }
}

impl std::convert::From<reqwest::Error> for Error {
    fn from(error: reqwest::Error) -> Error {
        Error::System(SystemError::HTTP(error.to_string()))
    }
}

pub mod constructors {
    use crate::error::*;

    pub fn file_not_found_error(body: &str) -> Error {
        Error::User(UserError::FileNotFound(body.to_string()))
    }

    pub fn file_conflict_error(body: &str) -> Error {
        Error::User(UserError::FileConflict(body.to_string()))
    }

    pub fn parsing_error(body: &str) -> Error {
        Error::User(UserError::Parsing(body.to_string()))
    }

    pub fn containerless_error(body: &str) -> Error {
        Error::System(SystemError::Containerless(body.to_string()))
    }

    pub fn io_error(body: &str) -> Error {
        Error::System(SystemError::IO(body.to_string()))
    }

    pub fn http_error(body: &str) -> Error {
        Error::System(SystemError::HTTP(body.to_string()))
    }
}