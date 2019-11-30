//! Manages a pool of containers that can process requests.
//! 
//! At the moment, the API is very straightforward and just has a single method
//! called `request`, which redirects a request to an available container. If no
//! container is immediately available, then `request` spawns a single new
//! container upto the configured maximum number of containers (i.e., cold
//! starts).

use super::container_handle::ContainerHandle;
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
use shared::config::InvokerConfig;
use std::sync::atomic;
use std::sync::Arc;
use std::time::{Duration, Instant};
use std::thread;
use nix::sys::signal::{kill, Signal};
use nix::unistd::Pid;

struct ContainerSpawner {
    container_name_suffix: usize,
    container_name_prefix: String,
    next_container_port: usize,
}

impl ContainerSpawner {
    /// Increments `container_name_suffix` and `next_container_port`.
    fn update(&mut self) {
        self.container_name_suffix += 1;
        self.next_container_port += 1;
    }

    // Creates a name for the next container.
    fn name(&self) -> String {
        return format!(
            "{}-{}",
            self.container_name_prefix, self.container_name_suffix
        );
    }
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
    tx_shutdown: Mutex<Option<futures::sync::oneshot::Sender<()>>>,
}

/// Creates a container.
/// 
/// 1. Creates `internal_authority` that publishes the docker port to a port
/// within the docker container.
/// 2. Creates `external_authority` that links the container host name and
/// docker port.
/// 3. Executes `docker run` using `internal_authoriy` and returns a
/// `ContainerHandle` using `external_authority`.
fn create_container(
    config: &Arc<InvokerConfig>,
    spawner: &mut MutexGuard<ContainerSpawner>,
) -> ContainerHandle {
    let name = spawner.name();
    let port = spawner.next_container_port;
    let internal_authority = format!("{}:{}", port, config.container_internal_port);
    let external_authority = Authority::from_shared(Bytes::from(format!(
        "{}:{}",
        &config.container_hostname, port
    )))
    .expect("error parsing external authority");
    spawner.update();
    let run_result = cmd!(
        "docker",
        "run",
        "-d", // detached, so return immediately
        "--rm", // delete on termination. Makes debugging harder
        "-p",
        internal_authority,
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
    let handle = ContainerHandle { name: name, authority: external_authority };
    return handle;
}

impl ContainerPoolData {
    pub fn new(
        config: Arc<InvokerConfig>,
        client: Arc<HttpClient>,
    ) -> (ContainerPoolData, futures::sync::oneshot::Receiver<()>) {
        let (tx_shutdown, rx_shutdown) = futures::sync::oneshot::channel::<()>();
        let tx_shutdown = Mutex::new(Some(tx_shutdown));
        let (idle, available) = Queue::new();
        let time_keeper = TimeKeeper::new(Duration::from_secs(30));
        let last_container_start_time = atomic::AtomicU64::new(util::unix_epoch_secs());
        let shutting_down = atomic::AtomicBool::new(false);
        let data = ContainerPoolData {
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

    /// Immediately stops all containers.
    pub fn stop_containers(this: &Arc<Self>) -> impl Future<Item = (), Error = Error> {
        let this2 = this.clone();
        this.available.recv().map(move |handle| {
            handle.stop();
            this2.num_containers.fetch_sub(1, SeqCst);
        })
    }

    /// Starts `n` new containers.
    /// 
    /// Creates and starts `n` new containers using [`create_container`](create_container).
    /// This creates `n` `ContainerHandle`s, which are added to idle using
    /// `idle.send(...)`.
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
            let handles = (0..n)
                .map(|_i| create_container(&config, &mut guard))
                .collect::<Vec<ContainerHandle>>();

            future::join_all(handles.into_iter().map(move |h| {
                let idle = idle.clone();
                h.test(client.clone())
                    .from_err()
                    .and_then(move |()| idle.send(h).from_err())
            }))
            .map(|_vec| ())
        });
        return future::Either::B(fut);
    }
}

/// A pool of containers.
#[derive(Clone)]
pub struct ContainerPool {
    data: Arc<ContainerPoolData>,
}

impl ContainerPool {
    /// Creates a container pool using `config` that handles requests using
    /// `client`.
    /// 
    /// Simultaneously launches a container killer.
    pub fn new(
        config: Arc<InvokerConfig>,
        client: Arc<HttpClient>,
    ) -> (ContainerPool, futures::sync::oneshot::Receiver<()>) {
        let (data, rx_shutdown) = ContainerPoolData::new(config.clone(), client);
        let data = Arc::new(data);
        ContainerPool::launch_container_killer(&config, data.clone());
        (ContainerPool { data }, rx_shutdown)
    }

    /// Spawns a future that periodically pings for containers to kill.
    /// 
    /// Every 1 second, the future checks to determine if all the containers can
    /// be killed. They can be killed if:
    /// 
    /// *(current_time - last_container_start_time) > min_container_lifespan*
    /// 
    /// &&
    /// 
    /// *time_keeper_mean < max_container_buffer_delay*
    /// 
    /// &&
    /// 
    /// *num_containers > 0*
    fn launch_container_killer(config: &InvokerConfig, data: Arc<ContainerPoolData>) {
        let max_container_buffer_delay = config.max_container_buffer_delay;
        let min_container_lifespan = config.min_container_lifespan;
        tokio::executor::spawn(
            util::set_interval(Duration::from_secs(1), move |_t| {
                let t = util::unix_epoch_secs();
                let n = data.last_container_start_time.load(SeqCst);
                let m = data.time_keeper.mean();
                let c = data.get_num_containers();
                if t - n < min_container_lifespan {
                    future::Either::B(future::ok(()))
                } else if m < max_container_buffer_delay && c > 0 {
                    future::Either::A(ContainerPoolData::stop_containers(&data))
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

    /// Shuts down the `ContainerPool`.
    /// 
    /// Creates a `Future` loop that loops through the containers and shuts them
    /// down when ready (i.e. when the the current request is completed) until
    /// all the containers are shut down.
    pub fn shutdown(&self) -> impl Future<Item = (), Error = Error> {
        let data = self.data.clone();
        self.data
            .tx_shutdown
            .lock()
            .from_err()
            .and_then(move |mut guard| {
                let tx_shutdown = std::mem::replace(&mut *guard, None);
                tx_shutdown
                    .expect("already called shutdown")
                    .send(())
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

    /// Shuts down the `ContainerPool`, if that `ContainerPool` was created from
    /// some parent.
    /// 
    /// 1. Sends a kill signal to the parent.
    /// 2. Sleeps for 4 seconds.
    /// 3. Shuts down, using a loop to kill all the containers when they are
    /// ready.
    pub fn shutdown_with_parent(&self) -> impl Future<Item = (), Error = Error> {
        let data = self.data.clone();
        kill(Pid::parent(), Signal::SIGUSR1).expect("Could not signal parent process");
        let goodnight = Duration::from_secs(4);
        thread::sleep(goodnight);
        self.data
            .tx_shutdown
            .lock()
            .from_err()
            .and_then(move |mut guard| {
                let tx_shutdown = std::mem::replace(&mut *guard, None);
                tx_shutdown
                    .expect("already called shutdown")
                    .send(())
                    .unwrap();
                data.shutting_down.store(true, SeqCst);
                // TODO(emily): There is something weird going on with this somewhere.
                // Not all of the containers get shut down...
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

    /// Sends a request to a container from the `ContainerPool`, creating
    /// additional containers as necessary.
    /// 
    /// 1. Retrieves a container, either by using an available one or by
    /// creating a new one.
    /// 2. Sends `req` to that container.
    /// 3. When it gets a response, it adds the container to idle...
    /// 4. ... and then returns the response.
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
                future::Either::B(
                    ContainerPoolData::create_containers(&data, 1)
                        .and_then(move |()| data.available.recv().from_err()),
                )
            }
        };
        container_fut.and_then(move |container| {
            container
                .request(data.client.clone(), req)
                .from_err()
                .and_then(move |resp| {
                    data.time_keeper.record_time(start.elapsed());
                    data.idle.send(container).from_err().map(move |()| resp)
                })
        })
    }
}
