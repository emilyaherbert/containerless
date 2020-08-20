use super::error::{*, constructors::*};
use hyper::Response;

pub fn ok_response(body: String) -> Result<std::result::Result<http::response::Response<std::string::String>, http::Error>, warp::Rejection> {
    Ok(Response::builder().status(200).body(body))
}

pub fn error_response(err: Error) -> Result<std::result::Result<http::response::Response<std::string::String>, http::Error>, warp::Rejection> {
    match err {
        Error::User(UserError::FileNotFound(body)) => Ok(Response::builder().status(210).body(body)),
        Error::User(UserError::FileConflict(body)) => Ok(Response::builder().status(211).body(body)),
        Error::User(UserError::Parsing(body)) => Ok(Response::builder().status(212).body(body)),
        Error::System(SystemError::IO(body)) => Ok(Response::builder().status(213).body(body)),
        Error::System(SystemError::HTTP(body)) => Ok(Response::builder().status(214).body(body)),
        Error::System(SystemError::Containerless(body)) => Ok(Response::builder().status(215).body(body))
    }
}

pub fn filter_errors_from_responses(status: http::StatusCode, body: String) -> Result<String, Error> {
    match status.as_u16() {
        200 => Ok(body),
        210 => Err(file_not_found_error(&body)),
        211 => Err(file_conflict_error(&body)),
        212 => Err(parsing_error(&body)),
        213 => Err(io_error(&body)),
        214 => Err(http_error(&body)),
        215 => Err(containerless_error(&body)),
        _ => panic!()
    }
}