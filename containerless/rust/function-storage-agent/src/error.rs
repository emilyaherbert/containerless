use std::io;

#[derive(Debug)]
pub enum Error {
    IO(String),
    FileNotFound(String),
    FileConflict(String),
}

impl std::convert::From<io::Error> for Error {
    fn from(error: io::Error) -> Error {
        Error::IO(error.to_string())
    }
}
