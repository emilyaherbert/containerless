use futures::sync::oneshot::Canceled;

#[derive(Debug)]
pub enum Error {
    Hyper(hyper::Error),
    Canceled,
    Unknown,
}

impl std::fmt::Display for Error {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Error::Hyper(err) => err.fmt(fmt),
            Error::Unknown => fmt.write_str("Unknown"),
            Error::Canceled => fmt.write_str("Canceled"),
        }
    }
}

impl std::error::Error for Error {}

impl std::convert::From<()> for Error {
    fn from(_error: ()) -> Error {
        return Error::Unknown;
    }
}

impl std::convert::From<hyper::Error> for Error {
    fn from(error: hyper::Error) -> Error {
        return Error::Hyper(error);
    }
}

impl std::convert::From<Canceled> for Error {
    fn from(_error: Canceled) -> Error {
        return Error::Canceled;
    }
}
