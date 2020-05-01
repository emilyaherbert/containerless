pub use super::trace_runtime::Containerless;
pub use futures::channel::mpsc;
pub use futures::channel::oneshot;
pub use futures::try_join;
pub use http::uri;
pub use log::{debug, error, info};
pub use std::collections::HashMap;
pub use std::convert::Into;
pub use std::str::FromStr;
pub use std::sync::atomic::Ordering::SeqCst;
pub use std::sync::atomic::{AtomicBool, AtomicI32, AtomicUsize};
pub use std::sync::{Arc, Weak};
pub use tokio::task;

// pub type Request = hyper::Request<hyper::Body>;
pub type Response = hyper::Response<hyper::Body>;
pub type K8sClient = Arc<k8s::client::Client>;
pub type HttpClient = Arc<hyper::Client<hyper::client::HttpConnector>>;
pub type HttpResponseResult = Result<Response, hyper::Error>;
