use std::io;

#[derive(Debug)]
pub enum Error {
    IO(io::Error),
    HTTP(reqwest::Error),
    Parsing(String),
    Storage(String),
    Compiler(String),
    Dispatcher(String),
    Kube(kube::Error),
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

impl std::convert::From<kube::Error> for Error {
    fn from(error: kube::Error) -> Error {
        Error::Kube(error)
    }
}