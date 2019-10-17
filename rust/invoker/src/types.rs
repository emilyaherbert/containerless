/// Aliases for types used throughout the program.
use hyper::client::HttpConnector;
use hyper::Client;
use hyper_rustls::HttpsConnector;
pub use serde_json::{Value as JsonValue};

pub type Request = hyper::Request<hyper::Body>;
pub type Response = hyper::Response<hyper::Body>;
pub type HttpClient = Client<HttpsConnector<HttpConnector>>;
