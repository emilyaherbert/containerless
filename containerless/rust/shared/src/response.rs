use hyper::Response;
use hyper::header::HeaderValue;

pub fn ok_response(
    body: String,
) -> Result<
    std::result::Result<http::response::Response<std::string::String>, http::Error>,
    warp::Rejection,
> {
    Ok(Response::builder().status(200).body(body))
}

pub fn ok_response_with_containerless_mode(
    body: String, mode: String,
) -> Result<
    std::result::Result<http::response::Response<std::string::String>, http::Error>,
    warp::Rejection,
> {
    let mut resp = Response::builder().status(200).body(body).unwrap();
    resp.headers_mut().insert(
        "X-Containerless-Mode",
        HeaderValue::from_str(&mode).unwrap(),
    );
    Ok(Ok(resp))
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
        _ => Err(format!("code {}, body {}", status_code, body)),
    }
}
