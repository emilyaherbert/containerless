use std::io;

#[derive(Debug)]
pub enum Error {
    IO(io::Error),
    HTTP(reqwest::Error),
    Parsing(String),
    Containerless(String)
}

impl std::convert::From<reqwest::Error> for Error {
    fn from(error: reqwest::Error) -> Error {
        Error::HTTP(error)
    }
}

impl std::convert::From<io::Error> for Error {
    fn from(error: io::Error) -> Error {
        Error::IO(error)
    }
}