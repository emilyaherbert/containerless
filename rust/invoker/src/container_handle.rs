use crate::types::*;
use duct::cmd;
use futures::Future;
use http::uri::Authority;
use std::sync::Arc;

// NOTE(emily, arjun): ContainerHandle must not implement Clone, since containers
// are shut down when dropped. If you really need multiple references to the
// same container, use Arc<ContainerHandle>.
#[derive(Debug)]
pub struct ContainerHandle {
    pub name: String,
    pub authority: Authority, // localhost:port
}

impl Drop for ContainerHandle {
    fn drop(&mut self) {
        // Note that the container may not be running
        self.stop();
    }
}

impl ContainerHandle {
    pub fn test(
        &self,
        client: Arc<HttpClient>,
    ) -> impl Future<Item = (), Error = tokio_retry::Error<hyper::Error>> {
        use tokio_retry::strategy::FixedInterval;
        use tokio_retry::Retry;

        let authority = Arc::new(self.authority.clone());

        let retry_strategy = FixedInterval::from_millis(1000).take(10);
        Retry::spawn(retry_strategy, move || {
            let req = hyper::Request::get(
                hyper::Uri::builder()
                    .scheme("http")
                    .authority((&*authority).clone())
                    .path_and_query("/")
                    .build()
                    .unwrap(),
            )
            .body(hyper::Body::empty())
            .unwrap();
            client.request(req)
        })
        // TODO(arjun): Check for a particular response?
        .map(|_resp| ())
    }

    pub fn request(
        &self,
        client: Arc<HttpClient>,
        mut req: Request,
    ) -> impl Future<Item = Response, Error = hyper::Error> {
        let _pq = (req.uri().path().to_string() + "?").to_string()
            + req.uri()
            .query()
            .map(move |q| {
                "?".to_string() + q.clone()
            })
            .unwrap_or("".to_string()).as_str();
        let new_uri = hyper::Uri::builder()
            .scheme("http")
            .authority(self.authority.clone())
            .path_and_query(req.uri().path())
            .build()
            .unwrap();
        *req.uri_mut() = new_uri;
        client.request(req)
    }

    pub fn stop(&self) {
        println!("Stopping {}", &self.name);
        let result = cmd!(
            "docker", "stop", "-t", "0", // stop immediately
            &self.name
        )
        .stdout_to_stderr()
        .run();
        if let Err(_) = result {
            eprintln!("Failed to stop the container {}", self.name);
        }
    }
}
