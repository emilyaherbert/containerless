pub use bytes::Bytes;
pub use futures::prelude::*;
pub use log::{debug, error, info};
pub use std::pin::Pin;
pub use std::sync::Arc;
pub use std::sync::atomic::{AtomicUsize, Ordering::SeqCst};
pub use tokio::task;
pub use std::collections::HashMap;
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
