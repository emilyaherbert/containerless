use super::container_handle::ContainerHandle;
/// Manages a pool of containers that can process requests. At the moment, the
/// API is very straightforward and just has a single method called `request`,
/// which redirects a request to an available container. If no container is
/// immediately available, then `request` spawns a single new container upto
/// the configured maximum number of containers (i.e., cold starts).
use shared::config::InvokerConfig;
use crate::error::Error;
use crate::mpmc::{Queue, Receiver, Sender};
use crate::time_keeper::TimeKeeper;
use crate::types::*;
use crate::util;
use atomic::Ordering::SeqCst;
use bytes::Bytes;
use duct::cmd;
use futures::{future, Future};
use futures_locks::{Mutex, MutexGuard};
use http::uri::Authority;
use std::sync::atomic;
use std::sync::Arc;
use std::time::{Duration, Instant};

struct ContainerSpawner {
    container_name_suffix: usize,
    container_name_prefix: String,
    next_container_port: usize,
}

struct ContainerPoolData {
    num_containers: atomic::AtomicUsize,
    // Use available.try_recv() to get a handle to a container that is ready
    // to process a request.
    available: Receiver<ContainerHandle>,
    // Once a container is done processing a request, use idle.send to make
    // it available to another thread.
    idle: Sender<ContainerHandle>,
    client: Arc<HttpClient>,
    config: Arc<InvokerConfig>,
    spawner: Mutex<ContainerSpawner>,
    time_keeper: TimeKeeper,
    last_container_start_time: atomic::AtomicU64,
    shutting_down: atomic::AtomicBool,
    tx_shutdown: Mutex<Option<futures::sync::oneshot::Sender<()>>>
}

#[derive(Clone)]
pub struct ContainerPool {
    data: Arc<ContainerPoolData>,
}

fn create_container(
    config: &Arc<InvokerConfig>,
    spawner: &mut MutexGuard<ContainerSpawner>,
) -> ContainerHandle {
    let name = format!(
        "{}-{}",
        spawner.container_name_prefix, spawner.container_name_suffix
    );
    spawner.container_name_suffix += 1;
    let port = spawner.next_container_port;
    let authority = Authority::from_shared(Bytes::from(format!(
        "{}:{}",
        &config.container_hostname, port
    )))
    .expect("error parsing authority");
    spawner.next_container_port += 1;
    let run_result = cmd!(
        "docker",
        "run",
        // detached, so return immediately
        "-d",
        // delete on termination. Makes debugging harder
        "--rm",
        "-p",
        format!("{}:{}", port, config.container_internal_port),
        "--name",
        &name,
        "--cpus",
        &config.cpus,
        "--memory",
        &config.memory,
        &config.image_name,
        // The remaining arguments are CLI arguments passed to the entrypoint
        // of the container
        "index.js",
        format!("{}", config.container_internal_port),
        "disable-tracing"
    )
    .stdout_to_stderr()
    .run();
    run_result.unwrap();
    let handle = ContainerHandle { name, authority };
    return handle;
}

impl ContainerPoolData {
    pub fn new(config: Arc<InvokerConfig>, client: Arc<HttpClient>) -> (ContainerPoolData, futures::sync::oneshot::Receiver<()>) {
        let (tx_shutdown, rx_shutdown) = futures::sync::oneshot::channel::<()>();
        let tx_shutdown = Mutex::new(Some(tx_shutdown));
        let (idle, available) = Queue::new();
        let time_keeper = TimeKeeper::new(Duration::from_secs(30));
        let last_container_start_time = atomic::AtomicU64::new(util::unix_epoch_secs());
        let shutting_down = atomic::AtomicBool::new(false);
        let data =  ContainerPoolData {
            num_containers: atomic::AtomicUsize::new(0),
            last_container_start_time,
            tx_shutdown,
            time_keeper,
            available,
            idle,
            client,
            config,
            shutting_down,
            spawner: Mutex::new(ContainerSpawner {
                container_name_suffix: 0,
                container_name_prefix: "vanilla".to_string(),
                next_container_port: 3000,
            }),
        };
        return (data, rx_shutdown);
    }

    fn get_num_containers(&self) -> usize {
        self.num_containers.load(SeqCst)
    }

    fn stop_container(this: &Arc<Self>) -> impl Future<Item = (), Error = Error> {
        let this2 = this.clone();
        this.available.recv().map(move |handle| {
            handle.stop();
            this2.num_containers.fetch_sub(1, SeqCst);
        })
    }

    // Starts `n` new containers.
    pub fn create_containers(this: &Arc<Self>, n: usize) -> impl Future<Item = (), Error = Error> {
        let idle = this.idle.clone();
        let config = this.config.clone();
        let client = this.client.clone();
        this.last_container_start_time
            .store(util::unix_epoch_secs(), SeqCst);

        // NOTE(arjun): The containers are not yet available. But, we increment
        // anyway to avoid exceeding self.max_containers
        if this.get_num_containers() == this.config.max_containers {
            return future::Either::A(future::ok(()));
        }

        this.num_containers.fetch_add(n, SeqCst);
        let fut = this.spawner.lock().from_err().and_then(move |mut guard| {
            // TODO Generate names, start new containers, add them to idle, and return
            // For robustness, we should send a request to each container to verify
            // that the server is running!
            let handles = (0..n)
                .map(|_i| create_container(&config, &mut guard))
                .collect::<Vec<ContainerHandle>>();

            future::join_all(handles.into_iter().map(move |h| {
                let idle = idle.clone();
                // NOTE(emily): This used to be h.clone().test(...), which
                // caused the old ones to be thrown out and drop(...) used. This
                // takes down the containers in docker.
                h.test(client.clone())
                    .from_err()
                    .and_then(move |()| idle.send(h).from_err())
            }))
            .map(|_vec| ())
        });
        return future::Either::B(fut);
    }
}

impl ContainerPool {
    pub fn new(config: Arc<InvokerConfig>, client: Arc<HttpClient>) -> (ContainerPool, futures::sync::oneshot::Receiver<()>) {
        let (data, rx_shutdown) = ContainerPoolData::new(config.clone(), client);
        let data = Arc::new(data);
        ContainerPool::launch_container_killer(&config, data.clone());
        (ContainerPool { data }, rx_shutdown)
    }

    fn launch_container_killer(config: &InvokerConfig, data: Arc<ContainerPoolData>) {
        let max_container_buffer_delay = config.max_container_buffer_delay;
        let min_container_lifespan = config.min_container_lifespan;
        tokio::executor::spawn(
            util::set_interval(Duration::from_secs(1), move |_t| {
                let t = util::unix_epoch_secs();
                if t - data.last_container_start_time.load(SeqCst) < min_container_lifespan {
                    future::Either::B(future::ok(()))
                } else if data.time_keeper.mean() < max_container_buffer_delay
                    && data.get_num_containers() > 0
                {
                    future::Either::A(ContainerPoolData::stop_container(&data))
                } else {
                    future::Either::B(future::ok(()))
                }
            })
            .map_err(|err| {
                println!("Error in launch_container_killer {}", err);
                std::process::exit(1)
            }),
        );
    }

    pub fn shutdown(&self) -> impl Future<Item = (), Error = Error> {
        let data = self.data.clone();
        self.data.tx_shutdown.lock().from_err().and_then(move |mut guard| {
            let tx_shutdown = std::mem::replace(&mut *guard, None);
            tx_shutdown.expect("already called shutdown").send(())
                .unwrap();
            data.shutting_down.store(true, SeqCst);
            future::loop_fn((), move |()| {
                let num_left = data.num_containers.fetch_sub(1, SeqCst);
                if num_left == 0 {
                    println!("num_left = {}", num_left);
                    return future::Either::A(future::ok(future::Loop::Break(())));
                } else {
                    let fut = data.available.recv().map(|container| {
                        container.stop();
                        future::Loop::Continue(())
                    });
                    return future::Either::B(fut);
                }
            })
        })
    }

    pub fn request(&self, req: Request) -> impl Future<Item = Response, Error = Error> {
        let start = Instant::now();
        let data = self.data.clone();
        // Either immediately get a container, or try to create one and wait
        // until it is available. Note that the create_containers function will
        // not actually create a container if we are at the configured limit.
        let container_fut = match data.available.recv_immediate() {
            Some(container) => future::Either::A(future::ok(container)),
            None => {
                let data = data.clone();
                future::Either::B(ContainerPoolData::create_containers(&data, 1)
                        .and_then(move |()| data.available.recv().from_err()))
            }
        };
        container_fut.and_then(move |container|
            container.request(data.client.clone(), req)
                .from_err()
                .and_then(move |resp| {
                    data.time_keeper.record_time(start.elapsed());
                    data.idle.send(container).from_err().map(move |()| resp)
                }))
    }
}
