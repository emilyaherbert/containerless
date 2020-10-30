pub use crate::trace_runtime::Containerless;
pub use futures::channel::mpsc;
pub use futures::channel::oneshot;
pub use futures::try_join;
pub use http::uri;
pub use log::{debug, error, info};
pub use std::collections::HashMap;
pub use std::convert::Into;
use std::fmt;
pub use std::str::FromStr;
pub use std::sync::atomic::Ordering::SeqCst;
pub use std::sync::atomic::{AtomicBool, AtomicI32, AtomicUsize};
pub use std::sync::{Arc, Weak};
pub use tokio::task;

// pub type Request = hyper::Request<hyper::Body>;
pub type Response = hyper::Response<hyper::Body>;
pub type K8sClient = Arc<k8s::client::Client>;
pub type HttpClient = Arc<hyper::Client<hyper_timeout::TimeoutConnector<hyper::client::HttpConnector>>>;
pub type HttpResponseResult = Result<Response, hyper::Error>;

pub struct SystemStatus {
    pub controller_service: bool,
    pub dispatcher_pod: bool,
    pub dispatcher_service: bool,
    pub storage_pod: bool,
    pub storage_service: bool,
    pub function_pods: HashMap<String, bool>,
    pub function_services: HashMap<String, bool>,
}

impl fmt::Display for SystemStatus {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut output_vec: Vec<String> = vec![];

        output_vec.push("controller:".to_string());
        if self.controller_service {
            output_vec.push("\t✓  service".to_string());
        } else {
            output_vec.push("\tX  service".to_string());
        }

        output_vec.push("\ndispatcher:".to_string());
        if self.dispatcher_service {
            output_vec.push("\t✓  service".to_string());
        } else {
            output_vec.push("\tX  service".to_string());
        }
        if self.dispatcher_pod {
            output_vec.push("\t✓  pod".to_string());
        } else {
            output_vec.push("\tX  pod".to_string());
        }

        output_vec.push("\nstorage:".to_string());
        if self.storage_service {
            output_vec.push("\t✓  service".to_string());
        } else {
            output_vec.push("\tX  service".to_string());
        }
        if self.storage_pod {
            output_vec.push("\t✓  pod".to_string());
        } else {
            output_vec.push("\tX  pod".to_string());
        }

        output_vec.push("\nTODO: function status stuff!".to_string());

        let output = output_vec.join("\n");
        write!(f, "{}", output)
    }
}
