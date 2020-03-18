use crate::autoscaler::Autoscaler;
use k8s;
use crate::types::*;
use futures::channel::oneshot;
use futures::lock::Mutex;
use std::collections::HashMap;
use std::sync::{Arc, Weak};
use regex::Regex;
use lazy_static::lazy_static;

struct FunctionTableImpl {
    functions: HashMap<String, Arc<Autoscaler>>,
    http_client: HttpClient,
    k8s_client: K8sClient,
}

pub struct FunctionTable {
    inner: Mutex<FunctionTableImpl>,
}

impl FunctionTable {
    pub async fn new() -> Arc<FunctionTable> {
        let functions = HashMap::new();
        let k8s_client = Arc::new(
            k8s::client::Client::new("containerless")
                .await
                .expect("initializing k8s client"),
        );
        let http_client = Arc::new(hyper::Client::new());
        let inner = FunctionTableImpl {
            functions,
            http_client,
            k8s_client,
        };
        return Arc::new(FunctionTable {
            inner: Mutex::new(inner),
        });
    }

    pub async fn adopt_running_functions(self_: &Arc<FunctionTable>) -> Result<(), kube::Error> {
        lazy_static! {
            static ref RE: Regex = Regex::new("^function-(.*)$").unwrap();
        }
        let mut inner = self_.inner.lock().await;
        let replica_sets = inner.k8s_client.list_replica_sets().await?;
        for (name, spec) in replica_sets.into_iter() {
            eprintln!("Found ReplicaSet {}", &name);
            match RE.captures(&name) {
                None => (),
                Some(captures) => {
                    let (send, recv) = oneshot::channel::<()>();
                    tokio::task::spawn(FunctionTable::cleanup_task(
                        Arc::downgrade(self_),
                        recv,
                        name.to_string(),
                    ));
                    let function_name = captures.get(1).unwrap().as_str();
                    let num_replicas = spec.replicas.unwrap() as usize;
                    eprintln!("Adopting {}", function_name);
                    let autoscaler = Autoscaler::adopt(
                        inner.k8s_client.clone(),
                        inner.http_client.clone(),
                        function_name.to_string(),
                        num_replicas,
                        send
                    );
                    inner.functions.insert(function_name.to_string(), autoscaler);
                }
            }
        }

        return Ok(());
    }

    pub async fn shutdown(&self) -> () {
        let mut inner = self.inner.lock().await;
        for (name, fm) in inner.functions.drain() {
            if let Err(err) = fm.shutdown().await {
                println!("Error shutting down {}: {}", name, err);
            }
        }
    }

    pub async fn shutdown_function(&self, name: &str) {
        let mut inner = self.inner.lock().await;
        match inner.functions.remove(name) {
            None => eprintln!("{} not in hash table", name),
            Some(fm) => {
                if let Err(err) = fm.shutdown().await {
                    eprintln!("error shutting down {}", err);
                }
            }
        }
    }

    async fn cleanup_task(self_: Weak<FunctionTable>, recv: oneshot::Receiver<()>, name: String) {
        if let Err(_cancelled) = recv.await {
            eprintln!("Cancelled");
        }
        if let Some(table) = self_.upgrade() {
            table.shutdown_function(&name).await;
        }
    }

    pub async fn get_function(self_: &Arc<FunctionTable>, name: &str) -> Arc<Autoscaler> {
        let mut inner = self_.inner.lock().await;
        match inner.functions.get(name) {
            None => {
                let (send, recv) = oneshot::channel::<()>();
                tokio::task::spawn(FunctionTable::cleanup_task(
                    Arc::downgrade(self_),
                    recv,
                    name.to_string(),
                ));
                let fm = Autoscaler::new(
                    inner.k8s_client.clone(),
                    inner.http_client.clone(),
                    name.to_string(),
                    send,
                )
                .await;
                inner.functions.insert(name.to_string(), fm.clone());
                return fm;
            }
            Some(value) => {
                return value.clone();
            }
        }
    }
}
