use crate::config::Config;
use crate::error::Error;
use crate::mpmc::{Queue, Receiver, Sender};
use crate::types::*;
use duct::cmd;
use std::time::{Duration, Instant};
use atomic::Ordering::SeqCst;
/// Manages a pool of containers that can process requests. There are three
/// primary functions in the API:
///
/// 1. `request` sends a request to a container and returns a response in
///    a future.
/// 2. `create_containers` adds new containers to the pool.
/// 3. `remove_containers` deletes containers from the pool.
///
/// The request function sends a
use futures::{future, Future};
use futures_locks::{Mutex, MutexGuard};
use std::sync::atomic;
use std::sync::Arc;

#[derive(Clone, Debug)]
struct ContainerHandle {
    name: String,
    authority: String, // localhost:port
}

impl ContainerHandle {
    pub fn test(
        &self,
        client: Arc<HttpClient>,
    ) -> impl Future<Item = (), Error = tokio_retry::Error<hyper::Error>> {
        use tokio_retry::strategy::FixedInterval;
        use tokio_retry::Retry;

        let authority = self.authority.clone();

        let retry_strategy = FixedInterval::from_millis(1000).take(10);
        Retry::spawn(retry_strategy, move || {
            let req = hyper::Request::get(
                hyper::Uri::builder()
                    .scheme("http")
                    // TODO(arjun): Create Authority in ContainerHandle
                    .authority(authority.as_bytes())
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
    ) -> impl Future<Item = (Response, Duration), Error = hyper::Error> {
        let start = Instant::now();
        let new_uri = hyper::Uri::builder()
            .scheme("http")
            // TODO(arjun): Create Authority in ContainerHandle
            .authority(self.authority.as_bytes())
            .path_and_query(req.uri().path())
            .build()
            .unwrap();
        *req.uri_mut() = new_uri;
        client.request(req).map(move |resp| (resp, start.elapsed()))
    }
}

struct ContainerSpawner {
    container_name_suffix: usize,
    container_name_prefix: String,
    next_container_port: usize,
}

pub struct ContainerPool {
    // True when a container is currently being spawned
    spawning: Arc<atomic::AtomicBool>,
    num_containers: Arc<atomic::AtomicUsize>,
    // Use available.try_recv() to get a handle to a container that is ready
    // to process a request.
    available: Receiver<ContainerHandle>,
    // Once a container is done processing a request, use idle.send to make
    // it available to another thread.
    idle: Sender<ContainerHandle>,
    client: Arc<HttpClient>,
    config: Arc<Config>,
    spawner: Mutex<ContainerSpawner>,
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
    let authority = format!("{}:{}", &config.container_hostname, port);
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
        &config.image_name
    )
    .run();
    run_result.unwrap();
    let handle = ContainerHandle { name, authority };
    return handle;
}

impl ContainerPool {
    pub fn new(config: Arc<Config>, client: Arc<HttpClient>) -> ContainerPool {
        let (idle, available) = Queue::new();
        let spawning = Arc::new(atomic::AtomicBool::new(false));
        return ContainerPool {
            num_containers: Arc::new(atomic::AtomicUsize::new(0)),
            spawning,
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

    pub fn is_spawning(&self) -> bool {
        return self.spawning.load(SeqCst);
    }

    pub fn get_num_containers(&self) -> usize {
        return self.num_containers.load(atomic::Ordering::SeqCst);
    }

    pub fn request(this: &Arc<Self>, req: Request) -> impl Future<Item = (Response, Duration), Error = Error> {
        let idle = this.idle.clone();
        let this = this.clone();
        return future::loop_fn(req, move |req| {
            let idle = idle.clone();
            let client = this.client.clone();
            if this.get_num_containers() == 0 {
                let fut = ContainerPool::create_containers(&this, 1);
                return future::Either::A(fut
                        .map(move |()| future::Loop::Continue(req)));
            }
            let fut = this.available.recv().from_err().and_then(move |container| {
                container
                .request(client, req)
                .from_err()
                .and_then(move |resp| idle.send(container).from_err().map(|()| resp))
            });
            return future::Either::B(fut.map(|r| future::Loop::Break(r)));
        });
    }

    // Starts `n` new containers.
    pub fn create_containers(this: &Arc<Self>, n: usize) -> impl Future<Item = (), Error = Error> {
        let idle = this.idle.clone();
        let config = this.config.clone();
        let client = this.client.clone();
        let spawning = this.spawning.clone();
        this.spawning.store(true, SeqCst);

        // NOTE(arjun): The containers are not yet available. But, we increment
        // anyway to avoid exceeding self.max_containers
        if this.num_containers.load(SeqCst) == this.config.max_containers {
            return future::Either::A(future::ok(()));
        }

        this.num_containers.fetch_add(n, atomic::Ordering::SeqCst);
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
            })).map(move |_vec|  {
                spawning.store(false, SeqCst);
            })
        });
        return future::Either::B(fut);
    }
}
