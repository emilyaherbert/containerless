use std::io;

#[derive(Debug)]
pub enum Error {
    IO(String),
    FileNotFound(String),
    FileConflict(String),
}

impl Error {
    pub fn info(&self) -> String {
        match self {
            Error::IO(info) => info.to_owned(),
            Error::FileNotFound(info) => info.to_owned(),
            Error::FileConflict(info) => info.to_owned()
        }
    }
}

impl std::convert::From<io::Error> for Error {
    fn from(error: io::Error) -> Error {
        Error::IO(error.to_string())
    }
}