use super::types::*;

#[derive(Copy, Clone)]
pub enum Mode {
    Tracing(usize),
    Vanilla,
    Decontainerized(Containerless),
}

impl std::fmt::Display for Mode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Mode::Decontainerized(_) => f.write_str("Decontainerized"),
            Mode::Tracing(_) => f.write_str("Tracing"),
            Mode::Vanilla => f.write_str("Vanilla"),
        }
    }
}

pub struct RequestPayload {
    pub method: http::Method,
    pub path_and_query: String,
    pub body: hyper::Body,
}

pub struct ServerlessRequest {
    pub payload: RequestPayload,
    pub send: oneshot::Sender<HttpResponseResult>,
}

pub enum Message {
    Request(ServerlessRequest),
    ExtractAndCompile(oneshot::Sender<Response>),
    GetMode(oneshot::Sender<Response>),
    Shutdown(oneshot::Sender<Result<(), crate::error::Error>>),
    Orphan,
}
