use std::io;
use std::string;

#[derive(Debug)]
pub enum Error {
    IO(io::Error),
    Parsing(String)
}

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