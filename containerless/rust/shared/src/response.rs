use hyper::Response;

pub fn ok_response(
    body: String,
) -> Result<
    std::result::Result<http::response::Response<std::string::String>, http::Error>,
    warp::Rejection,
> {
    Ok(Response::builder().status(200).body(body))
}

pub fn error_response(
    body: String,
) -> Result<
    std::result::Result<http::response::Response<std::string::String>, http::Error>,
    warp::Rejection,
> {
    Ok(Response::builder().status(500).body(body))
}

pub fn response_into_result(status_code: u16, body: String) -> Result<String, String> {
    match status_code {
        200 => Ok(body),
        _ => Err(body),
    }
}
