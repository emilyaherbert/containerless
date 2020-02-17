//! Pool of different means of isolation (i.e. with-containers and without-containers).
//! 
//! *How should we switch between containers and Rust?*
//! 
//! We assume that the container can be run in two modes, either vanilla or
//! tracing. On the first request, we start a tracing container. If additional
//! containers are needed, we start vanilla containers.  After the tracing
//! container receives N requests, we extract the trace, compile it to Rust
//! and start serving requests from Rust instead of containers.  After switching
//! to Rust, we let containers shut down naturally. The platform may send a
//! request to the containers, which will only occur if the Rust code encounters
//! an unexplored code path. By allowing the containers to shut down slowly, we
//! can fall back to containers more quickly. However, we keep track of how many
//! times we recompile the Rust code. If recompilation happens more than L times,
//! we give up compiling to Rust.

use super::container_handle::ContainerHandle;
use super::container_pool::ContainerPool;
use super::error::Error;
use super::trace_runtime::{Containerless, Decontainer};
use super::types;
use atomic_enum::atomic_enum;
use auto_enums::auto_enum;
use bytes::Bytes;
use duct::cmd;
use futures::future::Future;
use futures::stream::Stream;
use futures_cpupool::CpuPool;
use http::uri::Authority;
use shared::config::InvokerConfig;
use std::fs;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering::SeqCst};
use std::sync::Arc;
use futures::future;

#[atomic_enum]
#[derive(PartialEq)]
enum IsolationStatus {
    /// Cold start, we don't even have a container ready
    NotStarted,
    /// We gave up. The Rust code keeps getting requests
    /// that it cannot handle.
    Aborted,
    /// Currently tracing
    Tracing,
    /// Currently executing code in Rust
    Decontainerized,
    /// Currently compiling code to Rust
    Compiling,
    /// The decontainerized code has been compiled to Rust and another process
    /// has started. Once all pending requests are serviced, we will shut down
    /// this process.
    Draining,
}

struct IsolationPoolData {
    tracing_container_available: AtomicBool,
    num_traced_requests: AtomicUsize,
    mode: AtomicIsolationStatus,
    container_pool: ContainerPool,
    config: Arc<InvokerConfig>,
    container_handle: ContainerHandle,
    client: Arc<types::HttpClient>,
    cpu_pool: CpuPool,
    containerless: Containerless,
}

/// The pool that organizes the container pool, the tracing container, and the
/// generated Rust code.
#[derive(Clone)]
pub struct IsolationPool {
    data: Arc<IsolationPoolData>,
}

static TRACING_CONTAINER_PORT: usize = 2999;
static TRACING_CONTAINER_NAME: &'static str = "tracing";
static MAX_TRACED: usize = 100;

impl IsolationStatus {
    fn new(config: &InvokerConfig) -> Self {
        use shared::config::InitialState;
        match config.initial_state {
            InitialState::Tracing => IsolationStatus::NotStarted,
            InitialState::Decontainerized => IsolationStatus::Decontainerized,
            InitialState::DisableTracing => IsolationStatus::Aborted,
        }
    }
}

impl IsolationPoolData {
    /// Creates the isolation pool data.
    /// 
    /// 1. Creates a `ContainerPool`.
    /// 2. Sets up all the tracing container info but does not actually start
    /// the tracing container.
    fn new(
        config: Arc<InvokerConfig>,
        containerless: Containerless,
        client: Arc<types::HttpClient>,
    ) -> (IsolationPoolData, futures::sync::oneshot::Receiver<()>) {
        let tracing_container_available = AtomicBool::new(false);
        let num_traced_requests = AtomicUsize::new(0);
        let mode = AtomicIsolationStatus::new(IsolationStatus::new(&config));
        let (container_pool, rx_shutdown) = ContainerPool::new(config.clone(), client.clone());
        let cpu_pool = CpuPool::new(1);
        let authority = Authority::from_shared(Bytes::from(format!(
            "{}:{}",
            config.container_hostname, TRACING_CONTAINER_PORT
        )))
        .expect("error parsing authority");
        let container_handle = ContainerHandle {
            authority,
            name: TRACING_CONTAINER_NAME.to_string(),
        };
        return (
            IsolationPoolData {
                containerless,
                tracing_container_available,
                num_traced_requests,
                mode,
                container_pool,
                config,
                container_handle,
                client,
                cpu_pool,
            },
            rx_shutdown,
        );
    }

    // Starts the tracing container.
    fn start_tracing_container(&self) {
        let config = &self.config;
        let run_result = cmd!(
            "docker",
            "run", // detached, so return immediately
            "-d", // delete on termination. Makes debugging harder
            "--rm",
            "-p",
            format!(
                "{}:{}",
                TRACING_CONTAINER_PORT, self.config.container_internal_port
            ),
            "--name",
            TRACING_CONTAINER_NAME,
            "--cpus",
            &config.cpus,
            "--memory",
            &config.memory,
            &config.image_name,
            // The remaining arguments are CLI arguments passed to the entrypoint
            // of the container
            "traced.js",
            format!("{}", config.container_internal_port)
        )
        .stdout_to_stderr()
        .run();
        run_result.unwrap();
    }
}

impl IsolationPool {
    pub fn new(
        config: Arc<InvokerConfig>,
        containerless: Option<Containerless>,
        client: Arc<types::HttpClient>,
    ) -> (Self, futures::sync::oneshot::Receiver<()>) {
        let (data, rx_shutdown) = IsolationPoolData::new(config, containerless.unwrap(), client);
        let data = Arc::new(data);
        return (IsolationPool { data }, rx_shutdown);
    }

    /// Extracts the trace from the tracing container and compiles it to Rust.
    /// 
    /// 1. Sends a request to the tracing container and recieves a trace as
    /// bytes in the response.
    /// 2. Writes the trace to a file.
    /// 3. Compiles the trace to Rust.
    /// 4. Builds the generated Rust code.
    /// 5. Shuts down.
    fn extract_and_compile_trace(&self) -> impl Future<Item = (), Error = ()> {
        let req = hyper::Request::get(
            hyper::Uri::builder()
                .scheme("http")
                .authority(self.data.container_handle.authority.clone())
                .path_and_query("/trace")
                .build()
                .unwrap(),
        )
        .body(hyper::Body::empty())
        .unwrap();
        let data = self.data.clone();
        let data2 = self.data.clone();
        self.data
            .client
            .request(req)
            .and_then(|resp| resp.into_body().concat2())
            .map(move |chunk| chunk.iter().cloned().collect::<Vec<u8>>())
            .from_err()
            .and_then(move |trace| {
                data2.cpu_pool.spawn_fn(move || {
                    fs::write("trace.json", trace).expect("Failed to write trace.json");
                    cmd!(
                        "cargo",
                        "run",
                        "test-codegen",
                        "-i",
                        "../containerless-scaffold/trace.json",
                        "-o",
                        "../containerless-scaffold/src/containerless.rs"
                    )
                    .dir("../compiler")
                    .run()
                    .expect("Failed to compile the trace to Rust");
                    cmd!("cargo", "build", "--release")
                        .run()
                        .expect("Failed to compile the Rust code generated by the trace compiler");

                    data.mode.store(IsolationStatus::Draining, SeqCst);

                    if data.config.kill_parent {
                        return future::Either::A(data.container_pool.shutdown_with_parent());
                    } else {
                        return future::Either::B(data.container_pool.shutdown());
                    }
                })
            })
            .map_err(|err| {
                println!("{:?}", err);
                ()
            })
    }

    /// Issues a request, starting a new tracing container if needed.
    ///
    /// A state machine where the isolation status directs the request:
    /// * `Aborted` -> `ContainerPool`
    /// * `Decontainerized` -> create a `Decontainer` and use that.
    /// * `NotStarted` -> start the tracing container and use that.
    /// * `Compiling` -> `ContainerPool`
    /// * `Draining` -> `ContainerPool`
    #[auto_enum(futures01::Future)]
    pub fn request(
        &self,
        req: types::Request,
    ) -> impl Future<Item = types::Response, Error = Error> {
        let mode = &self.data.mode;
        loop {
            let m = mode.load(SeqCst);
            match m {
                IsolationStatus::Aborted => {
                    return self.data.container_pool.request(req);
                }
                // Send request. If there is an error due to unknown, switch to
                // NotStarted
                IsolationStatus::Decontainerized => {
                    let data = self.data.clone();
                    let (parts, body) = req.into_parts();
                    return body.concat2().from_err().and_then(move |body| {
                        Decontainer::new(
                            data.containerless,
                            data.client.clone(),
                            parts,
                            body.to_vec(),
                        )
                    });
                }
                // Try to set the mode to Tracing. If succcessful, this request is going
                // to launch the tracing container. Either way, loop to try to request
                // again.
                IsolationStatus::NotStarted => {
                    let was_not_started = mode.compare_and_swap(
                        IsolationStatus::NotStarted,
                        IsolationStatus::Tracing,
                        SeqCst,
                    );
                    if IsolationStatus::NotStarted != was_not_started {
                        continue;
                    }
                    self.data.start_tracing_container();
                    let data = self.data.clone();
                    let data2 = self.data.clone();
                    return self
                        .data
                        .container_handle
                        .test(data.client.clone())
                        .from_err()
                        .and_then(move |()| {
                            data.container_handle
                                .request(data.client.clone(), req)
                                .from_err()
                        })
                        .map(move |resp| {
                            data2.tracing_container_available.store(true, SeqCst);
                            return resp;
                        });
                }
                // Already tracing. Try to send the request to the tracing container.
                // If it is unavailable, send it to the container pool instead.
                // If the tracing container has received enough requests, this
                // triggers compilating and shuts down the tracing container.
                IsolationStatus::Tracing => {
                    let was_available = self
                        .data
                        .tracing_container_available
                        .compare_and_swap(true, false, SeqCst);
                    if was_available == true {
                        let n = self.data.num_traced_requests.fetch_add(1, SeqCst);
                        if n == MAX_TRACED {
                            self.data.mode.store(IsolationStatus::Compiling, SeqCst);
                            tokio::executor::spawn(self.extract_and_compile_trace());
                        }
                        let data = self.data.clone();
                        return data
                            .container_handle
                            .request(self.data.client.clone(), req)
                            .from_err()
                            .map(move |resp| {
                                if n == MAX_TRACED {
                                    data.container_handle.stop();
                                } else {
                                    data.tracing_container_available.store(true, SeqCst);
                                }
                                resp
                            });
                    } else {
                        return self.data.container_pool.request(req);
                    }
                }
                IsolationStatus::Compiling => {
                    return self.data.container_pool.request(req);
                }
                IsolationStatus::Draining => {
                    return self.data.container_pool.request(req);
                }
            }
        }
    }
}
