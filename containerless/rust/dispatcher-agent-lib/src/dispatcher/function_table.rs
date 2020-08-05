use super::function_manager::FunctionManager;
use super::types::*;
use futures::lock::Mutex;
use k8s;
use lazy_static::lazy_static;
use regex::Regex;
use std::collections::HashMap;

struct FunctionTableImpl {
    functions: HashMap<String, FunctionManager>,
    http_client: HttpClient,
    k8s_client: K8sClient,
}

pub struct FunctionTable {
    inner: Mutex<FunctionTableImpl>,
    decontainerized_functions: HashMap<&'static str, Containerless>,
    upgrade_pending: Arc<AtomicBool>,
}

impl FunctionTable {
    pub async fn new(
        decontainerized_functions: HashMap<&'static str, Containerless>,
    ) -> Arc<FunctionTable> {
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
        let upgrade_pending = Arc::new(AtomicBool::new(false));
        return Arc::new(FunctionTable {
            inner: Mutex::new(inner),
            decontainerized_functions,
            upgrade_pending,
        });
    }

    pub async fn adopt_running_functions(self_: &Arc<FunctionTable>) -> Result<(), kube::Error> {
        lazy_static! {
            static ref RE: Regex = Regex::new("^function-vanilla-(.*)$").unwrap();
        }
        let mut inner = self_.inner.lock().await;
        let replica_sets = inner.k8s_client.list_replica_sets().await?;
        let pods: Vec<String> = inner
            .k8s_client
            .list_pods()
            .await?
            .into_iter()
            .map(|snapshot| snapshot.name)
            .collect();
        for (rs_name, spec) in replica_sets.into_iter() {
            match RE.captures(&rs_name) {
                None => {
                    debug!(target: "dispatcher", "Ignoring ReplicaSet {}", &rs_name);
                }
                Some(captures) => {
                    debug!(target: "dispatcher", "Adopting ReplicaSet {}", &rs_name);
                    let name = captures.get(1).unwrap().as_str();
                    let num_replicas = spec.replicas.unwrap();
                    let is_tracing = pods.contains(&format!("function-tracing-{}", &rs_name));
                    let fm = FunctionManager::new(
                        inner.k8s_client.clone(),
                        inner.http_client.clone(),
                        Arc::downgrade(self_),
                        name.to_string(),
                        super::state::CreateMode::Adopt {
                            num_replicas,
                            is_tracing,
                        },
                        self_
                            .decontainerized_functions
                            .get(name)
                            .map(|ptrptr| *ptrptr),
                        self_.upgrade_pending.clone(),
                    )
                    .await;
                    inner.functions.insert(name.to_string(), fm.clone());
                }
            }
        }

        return Ok(());
    }

    pub async fn shutdown(self_: Arc<FunctionTable>, name: &str) {
        let mut inner = self_.inner.lock().await;
        match inner.functions.remove(name) {
            None => eprintln!("{} not in hash table", name),
            Some(mut fm) => {
                fm.shutdown().await;
            }
        }
    }

    pub async fn orphan(self_: Arc<FunctionTable>) {
        let mut inner = self_.inner.lock().await;
        // Unnecessary sequential awaits
        for (_name, function_manager) in inner.functions.drain() {
            function_manager.orphan().await;
        }
    }

    pub async fn get_function(self_: &Arc<FunctionTable>, name: &str) -> FunctionManager {
        let mut inner = self_.inner.lock().await;
        match inner.functions.get(name) {
            None => {
                let fm = FunctionManager::new(
                    inner.k8s_client.clone(),
                    inner.http_client.clone(),
                    Arc::downgrade(self_),
                    name.to_string(),
                    super::state::CreateMode::New,
                    self_
                        .decontainerized_functions
                        .get(name)
                        .map(|ptrptr| *ptrptr),
                    self_.upgrade_pending.clone(),
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

    fn extract_status(snapshot: k8s::types::SystemSnapshot) -> k8s::types::SystemStatus {

        unimplemented!()
    }

    /// Returns a snapshot of the current system.
    pub async fn system_status(self_: &Arc<FunctionTable>) -> Result<k8s::types::SystemStatus, kube::Error> {
        let inner = self_.inner.lock().await;
        let pods = inner.k8s_client.list_pods().await?;
        let services = inner.k8s_client.list_services().await?;
        error!(target: "dispatcher", "{:?}", pods);
        error!(target: "dispatcher", "{:?}", services);
        let mut pods_hm = HashMap::new();
        for pod in pods.into_iter() {
            pods_hm.insert(pod.name.clone(), pod);
        }
        let mut services_hm = HashMap::new();
        for service in services.into_iter() {
            services_hm.insert(service.name.clone(), service);
        }
        let snapshot = k8s::types::SystemSnapshot {
            pods: pods_hm,
            services: services_hm
        };
        Ok(FunctionTable::extract_status(snapshot))
    }

    /// Checks to ensure that the core system is in an okay status.
    /// This means that the system must have:
    /// 1. Running controller service.
    /// 2. Running dispatcher service and pod.
    /// 3. Running storage service and pod.
    pub async fn system_status_ok(self_: &Arc<FunctionTable>) -> Result<bool, kube::Error> {
        let status = FunctionTable::system_status(self_).await?;
        if status.controller_service && status.dispatcher_service && status.dispatcher_pod && status.storage_service && status.storage_pod {
            // TODO: check function status
            return Ok(true)
        }
        return Ok(false);
    }
}
