use crate::error::Error;
/// Manages a pool of containers that can process requests. There are three
/// primary functions in the API:
///
/// 1. `request` sends a request to a container and returns a response in
///    a future.
/// 2. `create_containers` adds new containers to the pool.
/// 3. `remove_containers` deletes containers from the pool.
///
/// The request function sends a
use crate::types::*;
use futures::{future, Future};
use futures_locks::{Mutex, MutexGuard};
use std::sync::Arc;
use crate::mpmc::{Receiver, Sender, Queue};
use duct::cmd;

#[derive(Clone, Debug)]
struct ContainerHandle {
    name: String,
    authority: String, // localhost:port
}

impl ContainerHandle {
    pub fn request(
        &self,
        client: Arc<HttpClient>,
        mut req: Request,
    ) -> impl Future<Item = Response, Error = hyper::Error> {
        let new_uri = hyper::Uri::builder()
            .scheme("http")
            .authority("localhost:3000")
            .path_and_query(req.uri().path())
            .build()
            .unwrap();
        *req.uri_mut() = new_uri;
        println!("Sending a request to {}", &self.name);
        client.request(req)
    }
}

struct ContainerSpawner {
    image_name: String,
    container_name_suffix: usize,
    container_name_prefix: String,
    next_container_port: usize,
}

pub struct ContainerPool {
    // Use available.try_recv() to get a handle to a container that is ready
    // to process a request.
    available: Receiver<ContainerHandle>,
    // Once a container is done processing a request, use idle.send to make
    // it available to another thread.
    idle: Sender<ContainerHandle>,
    client: Arc<HttpClient>,
    spawner: Mutex<ContainerSpawner>,
}

fn create_container(spawner: &mut MutexGuard<ContainerSpawner>) -> ContainerHandle {
    let name = format!(
        "{}-{}",
        spawner.container_name_prefix, spawner.container_name_suffix
    );
    spawner.container_name_suffix += 1;
    let port = spawner.next_container_port;
    let authority = format!("http://localhost:{}", port);
    spawner.next_container_port += 1;
    let run_result = cmd!(
        "docker",
        "run",
        // detached, so return immediately
        "-d",
        // delete on termination. Makes debugging harder
        "--rm",
        // NOTE(arjun): Hardcoded port 3000 as the listening port within the container
        "-p",
        format!("{}:3000", port),
        "--name",
        &name,
        &spawner.image_name
    )
    .run();
    run_result.unwrap();
    let handle = ContainerHandle { name, authority };
    return handle;
}

impl ContainerPool {
    pub fn new(client: Arc<HttpClient>) -> ContainerPool {
        let (idle, available) = Queue::new();
        return ContainerPool {
            available,
            idle,
            client,
            spawner: Mutex::new(ContainerSpawner {
                image_name: "demo-container".to_string(),
                container_name_suffix: 0,
                container_name_prefix: "vanilla".to_string(),
                next_container_port: 3000,
            }),
        };
    }

    pub fn request(&self, req: Request) -> impl Future<Item = Response, Error = Error> {
        let client = self.client.clone();
        let idle = self.idle.clone();
        self.available.recv().from_err().and_then(|container| {
            container
                .request(client, req)
                .from_err()
                .and_then(move |resp| {
                    idle.send(container).from_err().map(|()| resp)
                })
        })
    }

    // Starts `n` new containers.
    pub fn create_containers(&self, n: usize) -> impl Future<Item = (), Error = Error> {
        let idle = self.idle.clone();
        self.spawner.lock().from_err().and_then(move |mut guard| {
            // TODO Generate names, start new containers, add them to idle, and return
            // For robustness, we should send a request to each container to verify
            // that the server is running!
            let handles = (0..n)
                .map(|_i| create_container(&mut guard))
                .collect::<Vec<ContainerHandle>>();

            future::join_all(handles.into_iter().map(move |h| idle.send(h)))
                .from_err()
                .map(|_vec| ())
        })
    }
}
