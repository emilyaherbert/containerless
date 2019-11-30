//! The entry-point to the generated Rust code.
//! 
//! `Decontainer` implements `Future` and has a custom implementation of `poll`
//! that allows it to be used with the invoker server and incoming requests.

use super::super::error::Error;
use super::super::types::{self, HttpClient};
use super::error::Error as JSError;
use super::execution_context::*;
use super::type_dynamic::Dyn;
use super::Containerless;
use bumpalo::Bump;
use futures::{future, Async, Future, Poll};
use hyper::{Body, Response};
use serde_json::Value as JsonValue;
use std::convert::TryInto;
use std::pin::Pin;
use std::sync::Arc;

// This structure owns an arena and has an unsafe implementation of `Send`.
// In general, it is not safe to send `Bump` across threads without a mutex.
// However, the `poll` method in `Decontainer` ensures that only one thread
// accesses `bump` at a time, which is why this is safe.
unsafe impl Send for DecontainerImpl {}

struct Event {
    completed: bool,
    closure: Dyn<'static>,
    indicator: i32,
    future: Box<dyn Future<Item = AsyncOpOutcome, Error = JSError> + Send>,
}

impl Event {
    pub fn new(
        closure: Dyn<'static>,
        indicator: i32,
        future: Box<dyn Future<Item = AsyncOpOutcome, Error = JSError> + Send>,
    ) -> Self {
        let completed = false;
        Event {
            completed,
            closure,
            indicator,
            future,
        }
    }
}

// TODO(arjun): Does this need to have a `PhantomPin` field?
struct DecontainerImpl {
    arena: Bump,
    machine_state: Vec<Event>,
    ec: ExecutionContext<'static>,
    func: Containerless,
    request: Dyn<'static>,
    client: Arc<HttpClient>,
}

pub struct Decontainer {
    pinned: Pin<Box<DecontainerImpl>>,
}

impl Decontainer {
    pub fn new_from(
        func: Containerless,
        client: Arc<HttpClient>,
        path: &str,
        body: JsonValue,
    ) -> Decontainer {
        let arena = Bump::new();
        let request = Dyn::object(&arena);
        request.set_field("path", Dyn::str(&arena, path)).unwrap();
        let body_json = Dyn::from_json(&arena, body);
        request.set_field("body", body_json).unwrap();
        let request = unsafe { extend_lifetime(request) };
        let initial_event = Event::new(
            unsafe { extend_lifetime(Dyn::Int(0)) },
            0,
            Box::new(future::ok(AsyncOpOutcome::Initialize)),
        );

        return Decontainer {
            pinned: Box::pin(DecontainerImpl {
                arena,
                func,
                request,
                machine_state: vec![initial_event],
                ec: ExecutionContext::new(),
                client,
            }),
        };
    }

    pub fn new(
        func: Containerless,
        client: Arc<HttpClient>,
        parts: http::request::Parts,
        body: Vec<u8>,
    ) -> Decontainer {
        let body_json = serde_json::from_slice(&body).unwrap(); // TODO
        Decontainer::new_from(func, client, parts.uri.path(), body_json)
    }
}

unsafe fn shorten_invariant_lifetime<'b, 'c>(
    r: &'b mut ExecutionContext<'static>,
) -> &'b mut ExecutionContext<'c> {
    std::mem::transmute::<&'b mut ExecutionContext<'static>, &'b mut ExecutionContext<'c>>(r)
}

unsafe fn shorten_invariant_lifetime2<'a>(v: Dyn<'static>) -> Dyn<'a> {
    std::mem::transmute(v)
}

unsafe fn extend_lifetime<'a>(v: Dyn<'a>) -> Dyn<'static> {
    std::mem::transmute(v)
}

impl Future for Decontainer {
    type Item = types::Response;
    type Error = Error;

    fn poll<'a>(&'a mut self) -> Poll<Self::Item, Self::Error> {
        // Boilerplate to address pinning
        let mut_ref: std::pin::Pin<&'a mut DecontainerImpl> = Pin::as_mut(&mut self.pinned);
        let self_: &'a mut DecontainerImpl = unsafe { Pin::get_unchecked_mut(mut_ref) };

        assert!(self_.ec.new_ops.len() == 0);
        let mut ec: &'a mut ExecutionContext<'a> =
            unsafe { shorten_invariant_lifetime(&mut self_.ec) };
        loop {
            // Nothing left to do, so we are ready. Note that
            // Decontainer::new() adds a single future to machine_state, so
            // this is not empty initialy.
            if self_.machine_state.len() == 0 {
                if let Some(resp) = ec.response {
                    let resp: Result<String, ()> = resp.try_into();
                    if let Ok(body) = resp {
                        return Result::Ok(Async::Ready(Response::new(hyper::Body::from(body))));
                    }
                }
                // The follow error is not due to optimistic trace compilation.
                // It will re-occur if we re-execution in JavaScript, thus we
                // don't bother doing so.
                return Result::Ok(Async::Ready(
                    Response::builder()
                        .status(500)
                        .body(Body::from(format!("{:?}", ec.response)))
                        //.body(Body::from("Could not convert response to string (... from inside Rust...)"))
                        .unwrap(),
                ));
            }
            let mut any_completed = false;
            for event in self_.machine_state.iter_mut() {
                assert!(event.completed == false);
                match event.future.poll() {
                    Result::Err(err) => {
                        eprintln!("Error: {:?}", &err);
                        return Result::Err(Error::Unknown);
                    }
                    Result::Ok(Async::NotReady) => {
                        // Try next future
                    }
                    Result::Ok(Async::Ready(outcome)) => {
                        event.completed = true;
                        any_completed = true;
                        let f = &self_.func;
                        let args = outcome.process(
                            &self_.arena,
                            unsafe { shorten_invariant_lifetime2(self_.request) },
                            unsafe { shorten_invariant_lifetime2(event.closure) },
                        );
                        f(&self_.arena, &mut ec, Dyn::Int(event.indicator), args).unwrap();
                    }
                }
            }
            // Nothing completed, thus never executed T::callback.
            if any_completed == false {
                return Result::Ok(Async::NotReady);
            }
            self_.machine_state.retain(|event| event.completed == false);
            // Create a future for each new operation.
            for (op, indicator, clos) in ec.new_ops.drain(0..) {
                self_.machine_state.push(Event::new(
                    unsafe { extend_lifetime(clos) },
                    indicator,
                    Box::new(op.to_future(&self_.client)),
                ));
            }
        }
    }
}
