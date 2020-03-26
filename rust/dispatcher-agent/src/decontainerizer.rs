//! Decontainerizer
//!
//! Decontainerizer manages a single serverless function that may or may not
//! be running in a container. In principle, there are five possible states:
//!
//! - Tracing: A tracing container and an Autoscalar are available to process
//!   requests.
//! - Compiling: In the background, we are compiling traces to Rust. In this
//!   state, the tracing container is being shut down.
//! - RustAvailable: Rust code is available and the Autoscalar is being shut
//!   down.
//! - Decontainerized: Rust code is available and the Autoscalar is shut down.
//! - Aborted: We have given up on tracing. An Autoscalar is active.
//!
//! In all  states, there is either Rust code or an Autoscalar available
//! to process a request.
//!
//! An incoming request may trigger a transition from one state to another:
//!
//! - Tracing -> Compiling: After N requests are received.
//! - Compiling -> RustAvailable: When compilation completes and the Rust
//!   code pointer is available.
//! - RustAvailable -> Decontainerized: After the Autoscaler is shut down.
//! - Decontainerized -> Aborted: If we bail out of Rust code.
//!
//! The moment we enter a state, we perform the follow operations:
//! - Tracing: Start the tracing container. Note that the Autoscalar creates a
//!   FunctionManager, which also uses a task to create a Service and
//!   ReplicaSet. The FunctionManager can immediately buffer requests until the
//!   resources are ready.
//! - Compiling: Start compiling Rust code start shutting down the tracing
//!   container.  When compiling completes, update state to RustAvailable.
//! - RustAvailable: Start to shutdown the Autoscalar. When complete, update
//!   state to Decontainerized.
//! - Decontainerized: Nothing to do.
//! - Aborted: Restart the Autoscalar.
//!
//! The natural solution is to build a single executable per function, and have
//! the dispatcher start a new process per request (similar to CGI). This is
//! likely to be very slow. We could implement a FastCGI-style approach, but
//! that is another layer of management. (i.e., we would have to dynamically
//! determine when to start and stop each child process).
//!
//! Instead, we statically link all decontainerized functions into the
//! dispatcher.
use super::autoscaler::Autoscaler;
use super::types::*;
use futures::channel::oneshot;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use tokio::task;

enum State {
    /// The Rust code for the serverless function is statically linked to
    /// this version of the Dispatcher. The ReplicaSet and Service for the
    /// serverless function may still be alive. If so, shut them down.
    Decontainerized = 0,
    /// A tracing container and an Autoscalar are available to process requests.
    /// After N requests, extract the trace, send to the Controller, shutdown the
    /// tracing container, and transition to the Compiling state.
    Tracing = 1,
    /// The Controller is compiling the serverless function in the background.
    /// The Autoscalar is still available to process requests.
    Compiling = 2,
    /// Something went wrong. We should probably suppress the error, but to
    /// aide debugging, we fail closed.
    Error = 3,
}

impl From<usize> for State {
    fn from(val: usize) -> Self {
        match val {
            0 => State::Decontainerized,
            1 => State::Tracing,
            2 => State::Compiling,
            3 => State::Error,
            _ => unreachable!(),
        }
    }
}

pub struct Decontainerizer {
    name: String,
    k8s_client: K8sClient,
    state: AtomicUsize,
    autoscaler: Arc<Autoscaler>,
    decontainerized: (), // TODO
    tracing_pod_name: String,
}

impl Decontainerizer {
    async fn start_tracing_pod(self_: Arc<Decontainerizer>) -> () {
        let start_result = { self_.start_tracing_pod_internal().await };
        match start_result {
            Err(err) => {
                eprintln!("Error creating tracing Pod: {}", err);
                self_.state.store(State::Error as usize, Ordering::SeqCst);
                return;
            }
            Ok(()) => {
                return;
            }
        }
    }

    async fn start_tracing_pod_internal(&self) -> Result<(), kube::Error> {
        use k8s::builder::*;
        let pod = PodBuilder::new()
            .metadata(
                ObjectMetaBuilder::new()
                    .name(&self.tracing_pod_name)
                    .label("function", &self.name)
                    .label("mode", "tracing")
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
                            .env("FUNCTION_NAME", &self.name)
                            .env("FUNCTION_MODE", "tracing")
                            .build(),
                    )
                    .build(),
            )
            .build();

        self.k8s_client.new_pod(pod).await?;
        return Ok(());
    }

    async fn new_tracing_internal(
        name: String,
        k8s_client: K8sClient,
        autoscaler: Arc<Autoscaler>
    ) -> Arc<Decontainerizer> {

        let state = AtomicUsize::new(State::Tracing as usize);
        let decontainerized = ();
        let tracing_pod_name = format!("function-{}", &name);
        let decontainerizer = Decontainerizer {
            name,
            tracing_pod_name,
            k8s_client,
            autoscaler,
            state,
            decontainerized,
        };
        let decontainerizer = Arc::new(decontainerizer);
        task::spawn(Self::start_tracing_pod(decontainerizer.clone()));
        return decontainerizer;
    }


    pub async fn adopt(
        k8s_client: K8sClient,
        http_client: HttpClient,
        name: String,
        num_replicas: usize,
        on_zero_replicas: oneshot::Sender<()>,
    ) -> Arc<Decontainerizer> {

        let autoscaler = Autoscaler::adopt(
            k8s_client.clone(),
            http_client.clone(),
            name.clone(),
            num_replicas,
            on_zero_replicas,
        );
        let decontainerizer = Self::new_tracing_internal(
            name,
            k8s_client,
            autoscaler).await;
        return decontainerizer;
    }

    pub async fn new_tracing(
        name: String,
        k8s_client: K8sClient,
        http_client: HttpClient,
        on_zero_replicas: oneshot::Sender<()>,
    ) -> Arc<Decontainerizer> {
        let autoscaler = Autoscaler::new(
            k8s_client.clone(),
            http_client.clone(),
            name.clone(),
            on_zero_replicas,
        )
        .await;
        let decontainerizer = Self::new_tracing_internal(
            name,
            k8s_client,
            autoscaler).await;
        return decontainerizer;
    }

    pub async fn invoke(
        &self,
        method: http::Method,
        path_and_query: &str,
        body: hyper::Body,
    ) -> Result<Response, hyper::Error> {
        return self.autoscaler.invoke(method, path_and_query, body).await;
    }

    pub async fn shutdown(&self) -> Result<(), kube::Error> {
        self.autoscaler.shutdown().await?;
        self.k8s_client.delete_pod(&self.tracing_pod_name).await?;
        return Ok(());
    }

}
