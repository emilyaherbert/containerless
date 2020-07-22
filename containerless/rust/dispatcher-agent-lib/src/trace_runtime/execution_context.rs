use super::common::*;
use super::error::{type_error, Error};
use super::type_dynamic::{Dyn, DynObject, DynResult};
use std::convert::TryInto;

/// An enumeration of the events that a decontainerized function can run
/// within Rust.
#[derive(Debug)]
#[allow(unused)]
pub enum AsyncOp {
    Preinitialize,
    /// This event immediately completes, which is useful for testing without
    /// performing real I/O.
    Immediate,
    Listen,
    Request(String),
    Get(String),
    Post(String, Bytes),
}

pub enum AsyncOpOutcome {
    Initialize,
    Connected,
    GetResponse(Bytes),
    MockGetResponse(JsonValue),
}

impl AsyncOpOutcome {
    pub fn process<'a>(self, arena: &'a bumpalo::Bump, request: Dyn<'a>, clos: Dyn<'a>) -> Dyn<'a> {
        match self {
            AsyncOpOutcome::Initialize => {
                return clos;
            }
            AsyncOpOutcome::Connected => {
                let args = Dyn::vec(arena);
                args.push(clos).unwrap();
                args.push(request).unwrap();
                return args;
            }
            AsyncOpOutcome::MockGetResponse(json_value) => {
                let args = Dyn::vec(arena);
                args.push(clos).unwrap();
                args.push(Dyn::from_json(arena, json_value)).unwrap();
                return args;
            }
            AsyncOpOutcome::GetResponse(body) => {
                let args = Dyn::vec(arena);
                args.push(clos).unwrap();
                let resp = Dyn::from_json_string(arena, &String::from_utf8_lossy(&body))
                    .unwrap_or_else(|err| {
                        // TODO(arjun): Need DynResult as return type of this function
                        panic!("JSON error {} reading string {:?}", err, body);
                    });
                args.push(resp).unwrap();
                return args;
            }
        }
    }
}

impl AsyncOp {
    pub async fn to_future<'a>(self, client: &HttpClient) -> Result<AsyncOpOutcome, Error> {
        match self {
            AsyncOp::Preinitialize => return Ok(AsyncOpOutcome::Initialize),
            AsyncOp::Listen => {
                return Ok(AsyncOpOutcome::Connected);
            }
            AsyncOp::Post(url, body) => {
                if url.starts_with("data:") {
                    return Ok(AsyncOpOutcome::MockGetResponse(
                        serde_json::from_str(&url[5..])
                            .expect("malformed JSON in data: URL to POST"),
                    ));
                }
                use hyper::{Body, Request, Uri};
                let uri = Uri::from_str(&url)
                    .map_err(|_err| Error::TypeError("invalid URL in POST request".to_string()))?;
                let req = Request::builder()
                    .method("POST")
                    .uri(uri)
                    .body(Body::from(body))
                    .map_err(|_err| {
                        Error::TypeError("could not build request in POST".to_string())
                    })?;
                let resp_result = client.request(req).await;
                let resp = resp_result
                    .map_err(|err| Error::TypeError(format!("POST failed: {:?}", err)))?;
                let body = hyper::body::to_bytes(resp.into_body())
                    .await
                    .map_err(|_err| Error::TypeError("Reading response body".to_string()))?;
                return Ok(AsyncOpOutcome::GetResponse(body));
            }
            AsyncOp::Get(url) => {
                if url.starts_with("data:") {
                    return Ok(AsyncOpOutcome::MockGetResponse(
                        serde_json::from_str(&url[5..])
                            .expect("malformed JSON in data: URL to GET"),
                    ));
                }
                use hyper::{Body, Request, Uri};
                let uri = Uri::from_str(&url)
                    .map_err(|_err| Error::TypeError("invalid URL in POST request".to_string()))?;
                let req = Request::builder()
                    .uri(uri)
                    .body(Body::empty())
                    .map_err(|_err| {
                        Error::TypeError("could not build request in GET".to_string())
                    })?;
                let resp_result = client.request(req).await;
                let resp = resp_result
                    .map_err(|err| Error::TypeError(format!("GET failed: {:?}", err)))?;
                let body = hyper::body::to_bytes(resp.into_body())
                    .await
                    .map_err(|_err| Error::TypeError("Reading response body".to_string()))?;
                return Ok(AsyncOpOutcome::GetResponse(body));
            }
            _ => return Err(Error::TypeError("unimplemented".to_string())),
        }
    }
}

use futures::prelude::*;

pub struct PendingOp<'a> {
    async_op: AsyncOp,
    indicator: i32,
    closure: Dyn<'a>,
}

struct SendBox<T> {
    contents: T,
}
unsafe impl<T> Send for SendBox<T> {}

impl<'a> PendingOp<'a> {
    pub fn to_future2(
        self, client: &'a HttpClient,
    ) -> impl Future<Output = Result<(AsyncOpOutcome, i32, Dyn<'a>), Error>> + Send {
        let indicator = self.indicator;
        let closure = SendBox {
            contents: self.closure,
        };
        let op = self.async_op;
        op.to_future(client).map(move |result| match result {
            Err(err) => Err(err),
            Ok(outcome) => Ok((outcome, indicator, closure.contents)),
        })
    }

    pub fn initial() -> PendingOp<'a> {
        return PendingOp {
            async_op: AsyncOp::Preinitialize,
            indicator: 0,
            closure: Dyn::int(0),
        };
    }
}

/// The execution context allows a callback to send new events. The lifetime
/// `'a` is the lifetime of the arena in which the function may allocate
/// heap values.
pub struct ExecutionContext<'a> {
    pub new_ops: Vec<PendingOp<'a>>,
    pub response: Option<Dyn<'a>>,
    counter: usize,
}

impl<'a> ExecutionContext<'a> {
    pub fn new() -> Self {
        let response = None;
        let new_ops = vec![];
        let counter = 0;
        ExecutionContext {
            response,
            new_ops,
            counter,
        }
    }

    /// Send a new event. The argument `indicator` is sent back with the
    /// response, which helps the decontainerized keep track of multiple
    /// pending requests. There is no requirement that indicators be distinct,
    /// but that will be helpful to client code.
    pub fn loopback_int(&mut self, async_op: AsyncOp, indicator: i32, closure: Dyn<'a>) {
        self.new_ops.push(PendingOp {
            async_op,
            indicator,
            closure,
        });
    }

    pub fn loopback(
        &mut self, event_name: &'static str, event_arg: Dyn<'a>, event_clos: Dyn<'a>,
        indicator: i32,
    ) -> DynResult<'a> {
        if event_name == "listen" {
            self.loopback_int(AsyncOp::Listen, indicator, event_clos);
            return Ok(Dyn::int(0));
        } else if event_name == "get" {
            match event_arg.try_into() {
                Ok(url) => {
                    self.new_ops.push(PendingOp {
                        async_op: AsyncOp::Get(url),
                        indicator,
                        closure: event_clos,
                    });
                    return Ok(Dyn::int(0));
                }
                Err(()) => {
                    return type_error(format!("ec.loopback(\"get\", {:?}, ...)", event_arg));
                }
            }
        } else if event_name == "post" {
            match TryInto::<DynObject<'a>>::try_into(event_arg) {
                Ok(obj) => match (
                    TryInto::<String>::try_into(obj.get("url")),
                    obj.get("body").to_json(),
                ) {
                    (Ok(url), Some(body)) => {
                        let body = Bytes::from(
                            serde_json::to_vec(&body).expect("JSON serialization failed"),
                        );
                        self.new_ops.push(PendingOp {
                            async_op: AsyncOp::Post(url, body),
                            indicator,
                            closure: event_clos,
                        });
                        return Ok(Dyn::int(0));
                    }
                    _ => type_error("missing body/url to post"),
                },
                Err(()) => type_error(format!("ec.loopback(\"post\", {:?}, ...)", event_arg)),
            }
        } else {
            return type_error(format!(
                "unknown event name in ec.loopback({}, ...)",
                event_name
            ));
        }
    }

    pub fn send(&mut self, value: Dyn<'a>) -> DynResult<'a> {
        self.response = Some(value);
        Ok(Dyn::int(0))
    }

    #[allow(unused)]
    pub fn count(&mut self) -> Result<(), Error> {
        if self.counter == 10000 {
            Err(Error::OutOfGas)
        } else {
            self.counter += 1;
            Ok(())
        }
    }
}
