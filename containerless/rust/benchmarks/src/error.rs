use std::io;

#[derive(Debug)]
pub enum Error {
    IO(io::Error),
    FromUtf8Error(std::string::FromUtf8Error),
}

impl std::convert::From<io::Error> for Error {
    fn from(error: io::Error) -> Error {
        Error::IO(error)
    }
}

impl std::convert::From<std::string::FromUtf8Error> for Error {
    fn from(error: std::string::FromUtf8Error) -> Error {
        Error::FromUtf8Error(error)
    }
}
