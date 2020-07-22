use crate::types::*;
use futures::{channel::oneshot, lock::Mutex};
use std::collections::HashMap;
use std::sync::Arc;

pub type Containerless = fn() -> ();

#[derive(Debug, PartialEq)]
enum ServerlessFunctionMode {
    Tracing,
    Decontainerized(Containerless),
    Loading,
}

enum ServerlessFunctionState {
    Tracing,
    Decontainerized(Containerless),
    Loading(Vec<oneshot::Sender<()>>),
}

struct StateImpl {
    functions: HashMap<String, ServerlessFunctionState>,
}

#[derive(Clone)]
pub struct State {
    inner: Arc<Mutex<StateImpl>>,
}

impl StateImpl {
    fn new(decontainerized: Vec<(String, Containerless)>) -> StateImpl {
        // Populate with statically linked functions.
        let functions = decontainerized
            .into_iter()
            .map(|(name, rust_fn)| (name, ServerlessFunctionState::Decontainerized(rust_fn)))
            .collect();
        return StateImpl { functions };
    }
}

impl State {
    pub fn new(decontainerized: Vec<(String, Containerless)>) -> State {
        let inner = Arc::new(Mutex::new(StateImpl::new(decontainerized)));
        return State { inner };
    }

    pub async fn invoke<'a>(
        &self,
        function_name: &'a str,
        function_path: &'a str,
        method: hyper::Method,
        body: hyper::Body,
    ) -> Result<Response, hyper::Error> {
        let client = hyper::Client::new();
        let uri = hyper::Uri::builder()
            .scheme("http")
            .authority(function_name)
            .path_and_query(function_path)
            .build()
            .unwrap();
        let req = hyper::Request::builder()
            .method(method)
            .uri(uri)
            .body(body)
            .unwrap();

        panic!("foo");
    }

    pub async fn get(&self, name: &str) -> Option<ServerlessFunctionMode> {
        unimplemented!();
    }

    pub async fn set_loading(&self, name: &str) -> Option<oneshot::Sender<()>> {
        unimplemented!();
    }
}
