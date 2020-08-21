use thiserror::Error;
#[derive(Debug, Error)]
pub enum Error {
    #[error("{0}")]
    Kube(#[from] kube::Error),
    #[error("{0}")]
    Hyper(#[from] hyper::Error),
    #[error("{0}")]
    Reqwest(#[from] reqwest::Error),
    #[error("{0}")]
    Http(#[from] http::Error),
    #[error("Error::Timeout")]
    Timeout,
    #[error("pod {0} is in phase {0}")]
    UnexpectedPodPhase(String, k8s::PodPhase),
    #[error("TimeoutReason({0})")]
    TimeoutReason(String),
    #[error("communicating with controller: {0}")]
    Controller(String),
    #[error("communicating with storage: {0}")]
    Storage(String),
}

impl Error {
    pub fn controller<T>(message: impl Into<String>) -> Result<T, Self> {
        return Err(Error::Controller(message.into()));
    }
}
