use std::io;

#[derive(Debug)]
pub enum Error {
    IO(io::Error),
    Parsing(String),
    Compiler(String),
    Kube(kube::Error),
}

impl Error {
    /// This function makes printing slighlty prettier. Essentially it unwraps
    /// the core info about a message so that the internal error structure is
    /// not propogated and then shown to the user. It is not a super great
    /// system, and relies on the system hacker to understand what would qualify
    /// as a good error message and what wouldn't.
    pub fn info(&self) -> String {
        match self {
            Error::Compiler(info) => info.to_owned(),
            Error::Parsing(info) => info.to_owned(),
            error => format!("{:?}", error),
        }
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
