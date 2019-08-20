#[derive(Debug)]
pub enum Error {
    TypeError
}


impl std::fmt::Display for Error {

    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Error::TypeError => fmt.write_str("TypeError")
        }
    }

}

impl std::error::Error for Error { }
