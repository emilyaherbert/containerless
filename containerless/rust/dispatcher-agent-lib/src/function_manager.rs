use super::autoscaler::Autoscaler;
use super::error::*;
use super::function_table::FunctionTable;
use super::types::*;
use super::util;
use futures::prelude::*;
use hyper::header::HeaderValue;
use tokio::task;

#[derive(Debug, PartialEq)]
pub enum CreateMode {
    New,
    Adopt { num_replicas: i32, is_tracing: bool },
}

#[derive(Copy, Clone)]
enum Mode {
    Tracing(usize),
    Vanilla,
    Decontainerized(Containerless),
}

struct RequestPayload {
    method: http::Method,
    path_and_query: String,
    body: hyper::Body,
}

pub struct ServerlessRequest {
    payload: RequestPayload,
    send: oneshot::Sender<HttpResponseResult>,
}

enum Message {
    Request(ServerlessRequest),
    ExtractAndCompile(oneshot::Sender<Response>),
    GetMode(oneshot::Sender<Response>),
    Shutdown,
    Orphan,
}

struct State {
    name: String,
    k8s_client: K8sClient,
    http_client: HttpClient,
    tracing_pod_name: String,
    vanilla_name: String,
    tracing_pod_available: AtomicBool,
    tracing_authority: uri::Authority,
    vanilla_authority: uri::Authority,
}

#[derive(Clone)]
pub struct FunctionManager {
    send_requests: mpsc::Sender<Message>,
    state: Arc<State>,
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

impl State {
    fn new(name: String, k8s_client: K8sClient, http_client: HttpClient) -> Arc<Self> {
        let tracing_pod_name = format!("function-tracing-{}", &name);
        let vanilla_name = format!("function-vanilla-{}", &name);
        let tracing_pod_available = AtomicBool::new(true);
        let vanilla_authority =
            uri::Authority::from_str(&format!("{}:8081", &vanilla_name)).unwrap();
        let tracing_authority =
            uri::Authority::from_str(&format!("{}:8081", &tracing_pod_name)).unwrap();

        let state = State {
            name,
            k8s_client,
            http_client,
            tracing_pod_name,
            vanilla_name,
            tracing_pod_available,
            tracing_authority,
            vanilla_authority,
        };
        return Arc::new(state);
    }

    fn service_spec(&self, mode: &str) -> k8s_openapi::api::core::v1::ServiceSpec {
        use k8s::builder::*;
        return ServiceSpecBuilder::new()
            .add_port(ServicePortBuilder::new().name("http").port(8081).build())
            .add_port(ServicePortBuilder::new().name("manager").port(8080).build())
            .selector("function", &self.name)
            .selector("mode", mode)
            .build();
    }

    fn pod_spec(&self, mode: &str) -> k8s_openapi::api::core::v1::PodSpec {
        use k8s::builder::*;
        let builder = PodSpecBuilder::new().container(
            ContainerBuilder::new()
                .name("function")
                .image("localhost:32000/function-runner")
                .expose_port("manager", 8080)
                .expose_port("server", 8081)
                .pull_if_not_present()
                // A readiness probe ensures that we don't direct
                // requests to an instance until it is ready. By
                // default, wait ten seconds between each probe.
                .http_readiness_probe(1, "/readinessProbe", 8081)
                .env("FUNCTION_NAME", &self.name)
                .env("FUNCTION_MODE", mode)
                .build(),
        );
        if mode == "tracing" {
            // A crash should be an error in Containerless, and not an error
            // in user code. Restarting should be pointless.
            return builder.restart_never().build();
        } else {
            return builder.build();
        }
    }

    async fn start_vanilla_pod_and_service(&self) -> Result<(), Error> {
        use k8s::builder::*;
        let pod_template = PodTemplateSpecBuilder::new()
            .metadata(
                ObjectMetaBuilder::new()
                    .label("function", &self.name)
                    .label("mode", "vanilla")
                    .label("dynamic", "true")
                    .build(),
            )
            .spec(self.pod_spec("vanilla"))
            .build();

        let replica_set = ReplicaSetBuilder::new()
            .metadata(
                ObjectMetaBuilder::new()
                    .name(&self.vanilla_name)
                    .label("dynamic", "true")
                    .build(),
            )
            .spec(
                ReplicaSetSpecBuilder::new()
                    .replicas(1)
                    .selector(
                        LabelSelectorBuilder::new()
                            .match_label("function", &self.name)
                            .match_label("mode", "vanilla")
                            .match_label("dynamic", "true")
                            .build(),
                    )
                    .template(pod_template)
                    .build(),
            )
            .build();

        let service = ServiceBuilder::new()
            .metadata(
                ObjectMetaBuilder::new()
                    .name(&self.vanilla_name)
                    .label("dynamic", "true")
                    .build(),
            )
            .spec(self.service_spec("vanilla"))
            .build();

        self.k8s_client.new_replica_set(replica_set).await?;
        self.k8s_client.new_service(service).await?;
        util::wait_for_service(&self.http_client, self.vanilla_authority.clone()).await?;
        return Ok(());
    }

    async fn start_tracing_pod_and_service(&self) -> Result<(), Error> {
        use k8s::builder::*;
        let pod = PodBuilder::new()
            .metadata(
                ObjectMetaBuilder::new()
                    .name(&self.tracing_pod_name)
                    .label("function", &self.name)
                    .label("mode", "tracing")
                    .label("dynamic", "true")
                    .build(),
            )
            .spec(self.pod_spec("tracing"))
            .build();
        let service = ServiceBuilder::new()
            .metadata(
                ObjectMetaBuilder::new()
                    .name(&self.tracing_pod_name)
                    .label("dynamic", "true")
                    .build(),
            )
            .spec(self.service_spec("tracing"))
            .build();

        self.k8s_client.new_pod(pod).await?;
        self.k8s_client.new_service(service).await?;
        util::wait_for_pod_running(&self.k8s_client, &self.tracing_pod_name, 60).await?;
        return Ok(());
    }

    async fn send_trace_then_stop_pod_and_service(self_: Arc<Self>) -> Result<(), Error> {
        let req = hyper::Request::builder()
            .method("GET")
            .uri(format!("http://{}:8080/trace", &self_.tracing_pod_name))
            .body(hyper::Body::empty())
            .expect("constructing GET /trace");
        let resp = self_.http_client.request(req).await?;
        let req = hyper::Request::builder()
            .method("POST")
            .uri(format!("http://controller/recv_trace/{}", &self_.name))
            .body(resp.into_body())
            .expect("constructing POST /recv_trace");
        let resp = self_.http_client.request(req).await?;
        if resp.status() != 200 {
            return Error::controller(format!(
                "sending trace for {} to controller failed",
                self_.name
            ));
        }
        info!(target: "dispatcher", "removing Kubernetes resources for tracing {}", self_.name);
        try_join!(
            self_.k8s_client.delete_pod(&self_.tracing_pod_name),
            self_.k8s_client.delete_service(&self_.tracing_pod_name)
        )?;
        return Ok(());
    }

    async fn invoke_err(
        &self, authority: uri::Authority, serverless_request: ServerlessRequest,
        autoscaler: Arc<Autoscaler>, containerless_mode_header: &'static str,
    ) {
        let uri = hyper::Uri::builder()
            .scheme("http")
            .authority(authority)
            .path_and_query(serverless_request.payload.path_and_query.as_str())
            .build()
            .expect("constructing URI");
        debug!(target: "dispatcher", "issuing HTTP request to {}", &uri);
        autoscaler.recv_req();
        let req = hyper::Request::builder()
            .method(serverless_request.payload.method)
            // TODO(arjun): This is a bit of a kludge. We should probably pass
            // headers from the original request to the serverless function.
            // This is needed because the containerless library uses Express'
            // JSON bodyParser, which expects this header.
            .header("Content-Type", "application/json")
            .uri(uri)
            .body(serverless_request.payload.body)
            .expect("constructing request");
        let resp_result = self.http_client.request(req).await;
        autoscaler.recv_resp(); // decrement counter even if error
        let mut resp = match resp_result {
            Err(err) => {
                debug!(target: "dispatcher", "invoking {}", &err);
                hyper::Response::builder()
                    .status(500)
                    .body(hyper::Body::from("invoke error"))
                    .unwrap()
            }
            Ok(resp) => resp,
        };
        resp.headers_mut().insert(
            "X-Containerless-Mode",
            HeaderValue::from_static(containerless_mode_header),
        );

        util::send_log_error(serverless_request.send, Ok(resp));
    }

    async fn invoke_tracing(self_: Arc<Self>, req: ServerlessRequest, autoscaler: Arc<Autoscaler>) {
        self_.tracing_pod_available.store(false, SeqCst);
        Self::invoke_err(
            &self_,
            self_.tracing_authority.clone(),
            req,
            autoscaler,
            "tracing",
        )
        .await;
        self_.tracing_pod_available.store(true, SeqCst);
    }

    async fn invoke_vanilla(self_: Arc<Self>, req: ServerlessRequest, autoscaler: Arc<Autoscaler>) {
        Self::invoke_err(
            &self_,
            self_.vanilla_authority.clone(),
            req,
            autoscaler,
            "vanilla",
        )
        .await;
    }

    async fn invoke_decontainerized(self_: Arc<Self>, func: Containerless, req: ServerlessRequest) {
        // let data = req.payload.body.concat2();
        debug!(target: "dispatcher", "invoking decontainerized function {}", self_.name);
        let mut resp = match hyper::body::to_bytes(req.payload.body).await {
            Err(err) => hyper::Response::builder()
                .status(500)
                .body(hyper::Body::from(format!(
                    "error reading request payload from client {}",
                    err
                )))
                .unwrap(),
            Ok(body) => {
                match super::trace_runtime::run_decontainerized_function(
                    func,
                    self_.http_client.clone(),
                    &req.payload.path_and_query,
                    &body,
                )
                .await
                {
                    Err(err) => hyper::Response::builder()
                        .status(500)
                        .body(hyper::Body::from(format!(
                            "error from serverless function {}",
                            err
                        )))
                        .unwrap(),
                    Ok(resp) => resp,
                }
            }
        };
        resp.headers_mut().insert(
            "X-Containerless-Mode",
            HeaderValue::from_static("decontainerized"),
        );
        util::send_log_error(req.send, Ok(resp));
        // task::spawn(Self::invoke_decontainerized(Arc::clone(&self_), func, req));
    }

    async fn maybe_start_vanilla(
        self_: Arc<State>, create_mode: &CreateMode, containerless: Option<Containerless>,
    ) -> Result<(), Error> {
        if *create_mode == CreateMode::New && containerless.is_none() {
            return self_.start_vanilla_pod_and_service().await;
        }
        return Ok(());
    }

    async fn maybe_start_tracing(
        self_: Arc<State>, upgrade_pending: bool, create_mode: &CreateMode,
        containerless: Option<Containerless>,
    ) -> Result<(), Error> {
        if upgrade_pending {
            return Ok(());
        }
        if *create_mode == CreateMode::New && containerless.is_none() {
            return self_.start_tracing_pod_and_service().await;
        }
        return Ok(());
    }

    async fn function_manager_task(
        self_: Arc<State>, mut recv_requests: mpsc::Receiver<Message>,
        function_table: Weak<FunctionTable>, create_mode: CreateMode,
        containerless: Option<Containerless>, upgrade_pending: Arc<AtomicBool>,
    ) -> Result<(), Error> {
        try_join!(
            Self::maybe_start_tracing(
                self_.clone(),
                upgrade_pending.load(SeqCst),
                &create_mode,
                containerless
            ),
            Self::maybe_start_vanilla(self_.clone(), &create_mode, containerless)
        )?;

        let init_num_replicas = match create_mode {
            CreateMode::New => 1,
            CreateMode::Adopt {
                num_replicas,
                is_tracing: _,
            } => num_replicas,
        };

        let autoscaler = Autoscaler::new(
            Arc::clone(&self_.k8s_client),
            self_.vanilla_name.clone(),
            function_table,
            init_num_replicas,
            self_.name.clone(),
        );

        let mut mode = match containerless {
            None => Mode::Tracing(0),
            Some(f) => Mode::Decontainerized(f),
        };

        while let Some(message) = recv_requests.next().await {
            match (mode, message) {
                (_, Message::Orphan) => {
                    info!(target: "dispatcher", "orphaned Kubernetes resources for {}", self_.name);
                    autoscaler.terminate();
                    return Ok(());
                }
                (_, Message::Shutdown) => {
                    let is_tracing = match mode {
                        Mode::Tracing(_) => true,
                        _ => false,
                    };
                    info!(target: "dispatcher", "deleting Kubernetes resources for {}", self_.name);
                    try_join!(
                        util::maybe_run(
                            is_tracing,
                            self_.k8s_client.delete_pod(&self_.tracing_pod_name)
                        ),
                        util::maybe_run(
                            is_tracing,
                            self_.k8s_client.delete_service(&self_.tracing_pod_name)
                        ),
                        self_.k8s_client.delete_service(&self_.vanilla_name),
                        self_.k8s_client.delete_replica_set(&self_.vanilla_name)
                    )?;
                    if let Mode::Decontainerized(_) = mode {
                        // Do not terminate the task if decontainerized.
                        continue;
                    }
                    return Ok(());
                }
                (_, Message::GetMode(send)) => {
                    util::send_log_error(send, util::text_response(200, format!("{}", mode)));
                }
                (Mode::Decontainerized(func), Message::Request(req)) => {
                    let self_ = Arc::clone(&self_);
                    task::spawn(Self::invoke_decontainerized(self_, func, req));
                }
                (_, Message::ExtractAndCompile(send)) => {
                    if let Mode::Tracing(_) = mode {
                        {
                            let self_ = Arc::clone(&self_);
                            upgrade_pending.store(true, SeqCst);
                            task::spawn(util::log_error::<_, Error, _>(
                                async move {
                                    Self::send_trace_then_stop_pod_and_service(Arc::clone(&self_))
                                        .await?;
                                    util::send_log_error(
                                        send,
                                        util::text_response(
                                            200,
                                            "extracted and sent trace".to_string(),
                                        ),
                                    );
                                    return Ok(());
                                },
                                "extracting and sending trace",
                            ));
                        }
                        mode = Mode::Vanilla;
                        debug!(target: "dispatcher", "switched to Vanilla mode for {}", &self_.name);
                    } else {
                        util::send_log_error(
                            send,
                            util::text_response(403, "function is not tracing".to_string()),
                        );
                    }
                }
                (Mode::Tracing(5), Message::Request(req)) => {
                    task::spawn(Self::send_trace_then_stop_pod_and_service(Arc::clone(
                        &self_,
                    )));
                    mode = Mode::Vanilla;
                    debug!(target: "dispatcher", "switched to Vanilla mode for {}", &self_.name);
                    task::spawn(Self::invoke_vanilla(
                        Arc::clone(&self_),
                        req,
                        Arc::clone(&autoscaler),
                    ));
                }
                (Mode::Tracing(n), Message::Request(req)) => {
                    mode = Mode::Tracing(n + 1);
                    if self_.tracing_pod_available.load(SeqCst) {
                        task::spawn(Self::invoke_tracing(
                            Arc::clone(&self_),
                            req,
                            Arc::clone(&autoscaler),
                        ));
                    } else {
                        task::spawn(Self::invoke_vanilla(
                            Arc::clone(&self_),
                            req,
                            Arc::clone(&autoscaler),
                        ));
                    }
                }
                (Mode::Vanilla, Message::Request(req)) => {
                    task::spawn(Self::invoke_vanilla(
                        Arc::clone(&self_),
                        req,
                        Arc::clone(&autoscaler),
                    ));
                }
            }
        }

        return Ok(());
    }
}

impl FunctionManager {
    pub async fn new(
        k8s_client: K8sClient, http_client: HttpClient, function_table: Weak<FunctionTable>,
        name: String, create_mode: CreateMode, containerless: Option<Containerless>,
        upgrade_pending: Arc<AtomicBool>,
    ) -> FunctionManager {
        let (send_requests, recv_requests) = mpsc::channel(1);
        let err_msg = format!("error raised by task for {}", &name);
        let state = State::new(name, k8s_client, http_client);
        task::spawn(util::log_error(
            State::function_manager_task(
                Arc::clone(&state),
                recv_requests,
                function_table,
                create_mode,
                containerless,
                upgrade_pending,
            ),
            err_msg,
        ));
        let fm = FunctionManager {
            send_requests,
            state,
        };
        return fm;
    }

    pub async fn shutdown(&mut self) {
        self.send_requests
            .send(Message::Shutdown)
            .await
            .unwrap_or_else(|_| panic!("error sending Message::Shutdown for {}", self.state.name));
    }

    pub async fn orphan(mut self) {
        self.send_requests
            .send(Message::Orphan)
            .await
            .unwrap_or_else(|_| panic!("error sending Message::Orphan for {}", self.state.name));
    }

    pub async fn invoke(
        &mut self, method: http::Method, path_and_query: &str, body: hyper::Body,
    ) -> Result<Response, hyper::Error> {
        let (send_resp, recv_resp) = oneshot::channel();
        let req = ServerlessRequest {
            payload: RequestPayload {
                method,
                path_and_query: String::from(path_and_query),
                body,
            },
            send: send_resp,
        };
        error!(target: "dispatcher", "{}, {}, {}", self.state.name, self.state.tracing_pod_name, self.state.vanilla_name);
        self.send_requests
            .send(Message::Request(req))
            .await
            .unwrap();
        match recv_resp.await {
            Ok(result) => {
                return result;
            }
            Err(futures::channel::oneshot::Canceled) => {
                error!(target: "dispatcher", "dispatcher shutdown before before request for {} could be made", self.state.name);
                return Ok(hyper::Response::builder()
                    .status(500)
                    .body(hyper::Body::from("dispatcher shutdown"))
                    .unwrap());
            }
        }
    }

    pub async fn extract_and_compile(&mut self) -> Response {
        let (send_resp, recv_resp) = oneshot::channel();
        self.send_requests
            .send(Message::ExtractAndCompile(send_resp))
            .await
            .unwrap();
        match recv_resp.await {
            Ok(resp) => {
                return resp;
            }
            Err(oneshot::Canceled) => {
                return util::text_response(
                    500,
                    format!(
                        "dispatcher shutdown before trace could be extracted for {}",
                        self.state.name
                    ),
                );
            }
        }
    }

    pub async fn get_mode(&mut self) -> Response {
        let (send_resp, recv_resp) = oneshot::channel();
        self.send_requests
            .send(Message::GetMode(send_resp))
            .await
            .unwrap();
        match recv_resp.await {
            Ok(resp) => {
                return resp;
            }
            Err(oneshot::Canceled) => {
                return util::text_response(
                    500,
                    format!(
                        "dispatcher shutdown before mode could be queried for {}",
                        self.state.name
                    ),
                );
            }
        }
    }
}
