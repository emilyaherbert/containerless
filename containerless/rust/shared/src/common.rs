pub use bytes::Bytes;
pub use futures::prelude::*;
pub use log::{debug, error, info};
pub use std::collections::HashMap;
pub use std::pin::Pin;
pub use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering::SeqCst};
pub use std::sync::Arc;
pub use tokio::task;
pub static NAMESPACE: &'static str = "containerless";
pub use futures::channel::oneshot;

pub async fn suppress_and_log_err<F, E>(f: F)
where
    F: Future<Output = Result<(), E>>,
    E: std::error::Error,
{
    if let Err(err) = f.await {
        error!(target: "controller", "Error: {:?}", err);
    }
}

fn get_root_dir() -> String {
    let mut bin_path = std::env::current_exe().unwrap();
    assert!(bin_path.pop()); // pop controller-agent
    assert!(bin_path.pop()); // pop debug
    assert!(bin_path.pop()); // pop target
    return String::from(bin_path.as_path().to_str().unwrap());
}

lazy_static! {
    pub static ref ROOT: String = get_root_dir();
    pub static ref BUILD_VERSION: String = "release".to_string(); // make this an environment variable
}
