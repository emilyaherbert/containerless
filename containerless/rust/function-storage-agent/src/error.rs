use std::io;

#[derive(Debug)]
pub enum Error {
    IO(String),
    NotFound(String),
    Conflict(String),
}

impl Error {
    pub fn info(&self) -> String {
        match self {
            Error::IO(info) => info.to_owned(),
            Error::NotFound(info) => info.to_owned(),
            Error::Conflict(info) => info.to_owned(),
        }
    }
}

impl std::convert::From<io::Error> for Error {
    fn from(error: io::Error) -> Error {
        Error::IO(error.to_string())
    }
}
