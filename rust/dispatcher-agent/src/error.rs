#[derive(Debug)]
pub enum Error {
    Kube(kube::Error),
    Hyper(hyper::Error),
    Http(http::Error),
    Timeout,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::Kube(err) => err.fmt(f),
            Error::Hyper(err) => err.fmt(f),
            Error::Http(err) => err.fmt(f),
            Error::Timeout => f.write_str("Error::Timeout"),
        }
    }
}

impl std::error::Error for Error {}

impl From<kube::Error> for Error {
    fn from(err: kube::Error) -> Error {
        return Error::Kube(err);
    }
}

impl From<hyper::Error> for Error {
    fn from(err: hyper::Error) -> Error {
        return Error::Hyper(err);
    }
}

impl From<http::Error> for Error {
    fn from(err: http::Error) -> Error {
        return Error::Http(err);
    }
}
