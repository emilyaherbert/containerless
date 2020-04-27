use super::types::*;

pub struct ServerlessRequest {
    pub method: http::Method,
    pub path_and_query: String,
    pub body: hyper::Body,
    pub send: oneshot::Sender<HttpResponseResult>,
}

impl ServerlessRequest {
    pub fn with_receiver<S>(
        method: http::Method,
        path_and_query: S,
        body: hyper::Body,
    ) -> (Self, oneshot::Receiver<HttpResponseResult>)
    where
        S: Into<String>,
    {
        let (send, recv) = oneshot::channel();
        let req = Self {
            method: method,
            path_and_query: path_and_query.into(),
            body: body,
            send: send,
        };
        return (req, recv);
    }
}
