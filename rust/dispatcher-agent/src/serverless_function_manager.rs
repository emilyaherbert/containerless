use crate::types::*;
use futures::{channel::oneshot, lock::Mutex};
use std::sync::Arc;


enum ServerlessFunctionState {
    Tracing,
    Decontainerized(Containerless),
    Loading(Vec<oneshot::Sender<()>>),
}

struct ServerlessFunctionManagerImpl {
    name: String,
    authority: String,
    decontainerized: Option<()>,
    client: hyper::Client<hyper::client::HttpConnector>
}


impl ServerlessFunctionManagerImpl {
    
    pub async fn invoke(
        &self,
        path_and_query: &str,
        method: hyper::Method,
        body: hyper::Body,
    ) -> Result<Response, hyper::Error> {
        
        match self.decontainerized {        
            None => {
                let uri = hyper::Uri::builder()
                    .scheme("http")
                    .authority(self.authority.as_str())
                    .path_and_query(path_and_query)
                    .build()
                .unwrap();
                let req = hyper::Request::builder()
                    .method(method)
                    .uri(uri)
                    .body(body)
                    .unwrap();
                return self.client.request(req).await;
            },
            Some(rust_fn) => {
                unimplemented!();
            }
        }
    }
}

struct ServerlessFunctionManager {
    inner: Arc<Mutex<ServerlessFunctionManagerImpl>>
}
