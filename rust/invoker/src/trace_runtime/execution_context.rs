use super::super::types::{HttpClient, JsonValue};
use super::error::{type_error, Error};
use super::type_dynamic::{Dyn, DynResult};
use auto_enums::auto_enum;
use futures::{future, Future, Stream};
use std::convert::TryInto;
use std::sync::Arc;

/// An enumeration of the events that a decontainerized function can run
/// within Rust.
#[derive(Debug)]
pub enum AsyncOp {
    /// This event immediately completes, which is useful for testing without
    /// performing real I/O.
    Immediate,
    Listen,
    Request(String),
    Get(String),
}

pub enum AsyncOpOutcome {
    Initialize,
    Connected,
    GetResponse(hyper::Chunk),
    MockGetResponse(JsonValue)
}

impl AsyncOpOutcome {
    pub fn process<'a>(self, arena: &'a bumpalo::Bump, request: Dyn<'a>, clos: Dyn<'a>) -> Dyn<'a> {
        match self {
            AsyncOpOutcome::Initialize => {
                return clos;
            }
            AsyncOpOutcome::Connected => {
                let args = Dyn::vec(arena);
                args.push(clos);
                args.push(request);
                return args;
            }
            AsyncOpOutcome::MockGetResponse(json_value) => {
                let args = Dyn::vec(arena);
                args.push(clos);
                args.push(Dyn::from_json(arena, json_value));
                return args;
            },
            AsyncOpOutcome::GetResponse(body) => {
                let args = Dyn::vec(arena);
                args.push(clos);
                let resp = Dyn::from_json_string(arena, &String::from_utf8_lossy(&body))
                    .unwrap_or_else(|err| {
                        // TODO(arjun): Need DynResult as return type of this function
                        panic!("JSON error {} reading string {:?}", err, body);
                    });
                args.push(resp);
                return args;
            }
        }
    }
}

impl AsyncOp {
    #[auto_enum(futures01::Future)]
    pub fn to_future<'a>(
        self,
        client: &Arc<HttpClient>,
    ) -> impl Future<Item = AsyncOpOutcome, Error = Error> + Send {
        match self {
            AsyncOp::Listen => future::ok(AsyncOpOutcome::Connected),
            AsyncOp::Get(url) => {
                if url.starts_with("data:") {
                    return future::ok(
                        AsyncOpOutcome::MockGetResponse(
                            serde_json::from_str(&url[5..])
                                .expect("malformed JSON in data: URL to GET")));
                }
                use bytes::Bytes;
                use hyper::{Body, Request, Uri};
                let client = client.clone();
                let req = Uri::from_shared(Bytes::from(url))
                    .map_err(|_err| Error::TypeError("invalid URL in GET request".to_string()))
                    .and_then(|uri| {
                        Request::builder()
                            .uri(uri)
                            .body(Body::empty())
                            .map_err(|_err| {
                                Error::TypeError("could not build request in GET".to_string())
                            })
                    });
                return future::result(req)
                    .and_then(move |req| {
                        client
                            .request(req)
                            .map_err(|err| Error::TypeError(format!("GET failed: {:?}", err)))
                    })
                    .and_then(|resp| {
                        resp.into_body()
                            .concat2()
                            .map_err(|_err| Error::TypeError("Reading response body".to_string()))
                    })
                    .map(move |body| AsyncOpOutcome::GetResponse(body))
            }
            _ => future::err(Error::TypeError("unimplemented".to_string())),
        }
    }
}

/// The execution context allows a callback to send new events. The lifetime
/// `'a` is the lifetime of the arena in which the function may allocate
/// heap values.
pub struct ExecutionContext<'a> {
    pub new_ops: Vec<(AsyncOp, i32, Dyn<'a>)>,
    pub response: Option<Dyn<'a>>,
}

impl<'a> ExecutionContext<'a> {
    pub fn new() -> Self {
        let response = None;
        let new_ops = vec![];
        ExecutionContext { response, new_ops }
    }

    /// Send a new event. The argument `indicator` is sent back with the
    /// response, which helps the decontainerized keep track of multiple
    /// pending requests. There is no requirement that indicators be distinct,
    /// but that will be helpful to client code.
    pub fn loopback_int(&mut self, op: AsyncOp, indicator: i32, closure: Dyn<'a>) {
        self.new_ops.push((op, indicator, closure));
    }

    pub fn loopback(
        &mut self,
        event_name: &'static str,
        event_arg: Dyn<'a>,
        event_clos: Dyn<'a>,
        indicator: i32,
    ) -> DynResult<'a> {
        if event_name == "listen" {
            self.loopback_int(AsyncOp::Listen, indicator, event_clos);
            return Ok(Dyn::int(0));
        } else if event_name == "get" {
            match event_arg.try_into() {
                Ok(url) => {
                    self.new_ops
                        .push((AsyncOp::Get(url), indicator, event_clos));
                    return Ok(Dyn::int(0));
                }
                Err(()) => {
                    return type_error(format!("ec.loopback(\"get\", {:?}, ...)", event_arg));
                }
            }
        } else {
            return type_error(format!("ec.loopback({}, ...)", event_name));
        }
    }

    pub fn send(&mut self, value: Dyn<'a>) -> DynResult<'a> {
        self.response = Some(value);
        Ok(Dyn::int(0))
    }
}
