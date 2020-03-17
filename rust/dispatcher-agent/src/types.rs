use std::sync::Arc;

pub type Request = hyper::Request<hyper::Body>;
pub type Response = hyper::Response<hyper::Body>;
pub type K8sClient = Arc<k8s::client::Client>;
pub type HttpClient = Arc<hyper::Client<hyper::client::HttpConnector>>;
