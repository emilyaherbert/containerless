//! Function Manager
//!
//! Manages a single serverless function running on k8s. The current version
//! has several limitations:
//! 1. Runs a single pod, but uses a replicaSet, so scaling should be easy
//! 2. Does not support decontainerization
use crate::types::*;
use crate::util;
use futures::channel::oneshot;
use futures::lock::Mutex;
use http::uri;
use http::uri::Authority;
use std::str::FromStr;
use std::sync::atomic::{AtomicI32, AtomicUsize, Ordering};
use std::sync::Arc;
use tokio::task;
use k8s;

enum State {
    Loading = 0,
    Containerized = 1,
    Error = 2,
}

impl From<usize> for State {
    fn from(val: usize) -> Self {
        match val {
            0 => State::Loading,
            1 => State::Containerized,
            2 => State::Error,
            _ => unreachable!(),
        }
    }
}

pub struct FunctionManager {
    state: AtomicUsize,
    name: String,
    k8s: K8sClient,
    http_client: HttpClient,
    authority: uri::Authority,
    pending_requests: Mutex<Vec<PendingRequest>>,
    num_replicas: AtomicI32,
}

struct PendingRequest {
    method: http::Method,
    path_and_query: String,
    body: hyper::Body,
    send: oneshot::Sender<Result<Response, hyper::Error>>,
}

impl PendingRequest {
    pub fn new(
        send: oneshot::Sender<Result<Response, hyper::Error>>,
        method: http::Method,
        path_and_query: &str,
        body: hyper::Body,
    ) -> Self {
        return PendingRequest {
            method: method,
            path_and_query: path_and_query.to_string(),
            body: body,
            send: send,
        };
    }
}

impl FunctionManager {

    fn internal_create(
        k8s: K8sClient, 
        client: HttpClient,
        name: String,
        num_replicas: i32,
        state: State) -> Arc<FunctionManager> {
        let num_replicas = AtomicI32::new(num_replicas);
        let fm = FunctionManager {
            authority: Authority::from_str(&format!("function-{}:8081", &name)).unwrap(),
            state: AtomicUsize::new(state as usize),
            name: name,
            k8s: k8s.clone(),
            http_client: client.clone(),
            pending_requests: Mutex::new(Vec::new()),
            num_replicas,
        };
        return Arc::new(fm);
    }

    pub fn adopt(k8s: K8sClient, client: HttpClient, name: String, num_replicas: i32) -> Arc<FunctionManager> {
        return Self::internal_create(k8s, client, name, num_replicas, State::Containerized);
    }

    // May fail if the function does not exist.
    pub async fn new(k8s: K8sClient, client: HttpClient, name: String) -> Arc<FunctionManager> {
        let fm = Self::internal_create(k8s, client, name, 1, State::Loading);
        task::spawn(FunctionManager::load(fm.clone()));
        return fm;
    }

    async fn init_k8s(&self, name: String) -> Result<(), kube::Error> {
        use k8s::builder::*;
        let pod_template = PodTemplateSpecBuilder::new()
            .metadata(
                ObjectMetaBuilder::new()
                    .label("function", &name)
                    .label("mode", "vanilla")
                    .build(),
            )
            .spec(
                PodSpecBuilder::new()
                    .container(
                        ContainerBuilder::new()
                            .name("function")
                            .image("localhost:32000/function-runner")
                            .expose_port("manager", 8080)
                            .expose_port("server", 8081)
                            .always_pull()
                            // A readiness probe ensures that we don't direct
                            // requests to an instance until it is ready. By
                            // default, wait ten seconds between each probe.
                            .http_readiness_probe("/", 8081)
                            .env("FUNCTION_NAME", &name)
                            .env("FUNCTION_MODE", "vanilla")
                            .build(),
                    )
                    .build(),
            )
            .build();

        let replica_set = ReplicaSetBuilder::new()
            .metadata(
                ObjectMetaBuilder::new()
                    .name(format!("function-{}", &name))
                    .build(),
            )
            .spec(
                ReplicaSetSpecBuilder::new()
                    .replicas(1)
                    .selector(
                        LabelSelectorBuilder::new()
                            .match_label("function", &name)
                            .match_label("mode", "vanilla")
                            .build(),
                    )
                    .template(pod_template)
                    .build(),
            )
            .build();

        let service = ServiceBuilder::new()
            .metadata(
                ObjectMetaBuilder::new()
                    .name(format!("function-{}", &name))
                    .build(),
            )
            .spec(
                ServiceSpecBuilder::new()
                    .add_port(ServicePortBuilder::new().name("http").port(8081).build())
                    .selector("function", &name)
                    .build(),
            )
            .build();

        self.k8s.new_replica_set(replica_set).await?;
        self.k8s.new_service(service).await?;
        return Ok(());
    }

    async fn load(self_: Arc<FunctionManager>) -> () {
        let create_result = { self_.init_k8s(self_.name.clone()).await };
        match create_result {
            Err(err) => {
                eprintln!("Error creating ReplicaSet: {}", err);
                self_.state.store(State::Error as usize, Ordering::SeqCst);
                return;
            }
            Ok(()) => {
                let service_ping_uri = http::Uri::builder()
                    .scheme("http")
                    .authority(self_.authority.clone())
                    .path_and_query("/")
                    .build()
                    .unwrap();
                if let Err(_err) = util::retry_get(&self_.http_client, 5, 2, service_ping_uri).await
                {
                    eprintln!("Error waiting for service");
                    self_.state.store(State::Error as usize, Ordering::SeqCst);
                    return;
                }
                self_
                    .state
                    .store(State::Containerized as usize, Ordering::SeqCst);
                // Note that we update the state to Containerized before
                // acquiring the lock on pending_requests. A concurrent thread
                // issuing a request will thus issue requests directly, and
                // nothing will be added to pending_requests.

                // TODO(arjun): We should turn pending_requests into an Option
                // and set it to None, so that we don't lose any requests.
                let mut pending_requests = self_.pending_requests.lock().await;
                for pending_request in pending_requests.drain(0..) {
                    let uri = hyper::Uri::builder()
                        .scheme("http")
                        .authority(self_.authority.clone())
                        .path_and_query(pending_request.path_and_query.as_str())
                        .build()
                        .expect("building URI");
                    let req = hyper::Request::builder()
                        .method(pending_request.method)
                        .uri(uri)
                        .body(pending_request.body)
                        .expect("building request");
                    let _ = pending_request
                        .send
                        .send(self_.http_client.request(req).await);
                }
            }
        }
    }

    pub async fn invoke(
        &self,
        method: http::Method,
        path_and_query: &str,
        body: hyper::Body,
    ) -> Result<Response, hyper::Error> {
        let state = State::from(self.state.load(Ordering::SeqCst));
        match state {
            State::Loading => {
                let (send, recv) = oneshot::channel();
                {
                    let mut pending_requests = self.pending_requests.lock().await;
                    pending_requests.push(PendingRequest::new(send, method, path_and_query, body));
                }
                match recv.await {
                    Err(oneshot::Canceled) => {
                        return Ok(hyper::Response::builder()
                            .status(500)
                            .body(hyper::Body::from("dispatcher shutdown"))
                            .unwrap());
                    }
                    Ok(result) => {
                        return result;
                    }
                }
            }
            State::Error => {
                let resp = hyper::Response::builder()
                    .status(500)
                    .body(hyper::Body::from("function in an error state"))
                    .unwrap();
                return Ok(resp);
            }
            State::Containerized => {
                let uri = hyper::Uri::builder()
                    .scheme("http")
                    .authority(self.authority.clone())
                    .path_and_query(path_and_query)
                    .build()
                    .expect("building URI");
                let req = hyper::Request::builder()
                    .method(method)
                    .uri(uri)
                    .body(body)
                    .expect("building request");
                return self.http_client.request(req).await;
            }
        }
    }

    pub fn num_replicas(&self) -> i32 {
        return self.num_replicas.load(Ordering::SeqCst);
    }

    pub async fn set_replicas(&self, n: i32) -> Result<(), kube::Error> {
        use k8s::builder::*;
        assert!(n > 0, "number of replicas must be greater than zero");
        self.num_replicas.store(n, Ordering::SeqCst);
        let rs = ReplicaSetBuilder::new()
            .metadata(
                ObjectMetaBuilder::new()
                    .name(format!("function-{}", self.name))
                    .build(),
            )
            .spec(ReplicaSetSpecBuilder::new().replicas(n).build())
            .build();
        return self.k8s.patch_replica_set(rs).await;
    }
}
