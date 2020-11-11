use std::io;

#[derive(Debug)]
pub enum Error {
    IO(io::Error),
    Timeout,
    FromUtf8Error(std::string::FromUtf8Error),
    ParseIntError(std::num::ParseIntError),
    Containerless(shared::containerless::error::Error),
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

impl std::convert::From<std::num::ParseIntError> for Error {
    fn from(error: std::num::ParseIntError) -> Error {
        Error::ParseIntError(error)
    }
}

impl std::convert::From<shared::containerless::error::Error> for Error {
    fn from(error: shared::containerless::error::Error) -> Error {
        Error::Containerless(error)
    }
}
