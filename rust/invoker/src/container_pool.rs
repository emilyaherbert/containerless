/// Manages a pool of containers that can process requests. At the moment, the
/// API is very straightforward and just has a single method called `request`,
/// which redirects a request to an available container. If no container is
/// immediately available, then `request` spawns a single new container upto
/// the configured maximum number of containers (i.e., cold starts).
use crate::config::Config;
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

#[derive(Clone, Debug)]
struct ContainerHandle {
    name: String,
    authority: Authority, // localhost:port
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
        cmd!(
            "docker", "stop", "-t", "0", // stop immediately
            &self.name
        )
        .stdout_to_stderr()
        .run()
        .expect("failed to stop container");
    }
}

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
    config: Arc<Config>,
    spawner: Mutex<ContainerSpawner>,
    time_keeper: TimeKeeper,
    last_container_start_time: atomic::AtomicU64,
}

#[derive(Clone)]
pub struct ContainerPool {
    data: Arc<ContainerPoolData>,
}

fn create_container(
    config: &Arc<Config>,
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
    pub fn new(config: Arc<Config>, client: Arc<HttpClient>) -> ContainerPoolData {
        let (idle, available) = Queue::new();
        let time_keeper = TimeKeeper::new(Duration::from_secs(30));
        let last_container_start_time = atomic::AtomicU64::new(util::unix_epoch_secs());
        return ContainerPoolData {
            num_containers: atomic::AtomicUsize::new(0),
            last_container_start_time,
            time_keeper,
            available,
            idle,
            client,
            config,
            spawner: Mutex::new(ContainerSpawner {
                container_name_suffix: 0,
                container_name_prefix: "vanilla".to_string(),
                next_container_port: 3000,
            }),
        };
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
                h.clone()
                    .test(client.clone())
                    .from_err()
                    .and_then(move |()| idle.send(h).from_err())
            }))
            .map(|_vec| ())
        });
        return future::Either::B(fut);
    }
}

impl ContainerPool {
    pub fn new(config: Arc<Config>, client: Arc<HttpClient>) -> ContainerPool {
        let data = Arc::new(ContainerPoolData::new(config.clone(), client));
        ContainerPool::launch_container_killer(&config, data.clone());
        ContainerPool { data }
    }

    fn launch_container_killer(config: &Config, data: Arc<ContainerPoolData>) {
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

    pub fn request(&self, req: Request) -> impl Future<Item = Response, Error = Error> {
        let data = self.data.clone();
        match data.available.recv_immediate() {
            Some(container) => {
                let fut = container
                    .request(data.client.clone(), req)
                    .from_err()
                    .and_then(move |resp| {
                        data.idle.send(container).from_err().map(move |()| {
                            data.time_keeper.record_time(Duration::new(0, 0));
                            resp
                        })
                    });
                future::Either::B(fut)
            }
            None => {
                let start = Instant::now();
                let data2 = data.clone();
                let fut = ContainerPoolData::create_containers(&data, 1)
                    .and_then(move |()| data2.available.recv().from_err())
                    .and_then(move |container| {
                        container
                            .request(data.client.clone(), req)
                            .from_err()
                            .and_then(move |resp| {
                                data.idle.send(container).from_err().map(move |()| {
                                    data.time_keeper.record_time(start.elapsed());
                                    resp
                                })
                            })
                    });
                future::Either::A(fut)
            }
        }
    }
}
