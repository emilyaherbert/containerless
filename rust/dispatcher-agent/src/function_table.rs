use crate::autoscaler::Autoscaler;
use crate::function_manager::FunctionManager;
use crate::k8s;
use crate::types::*;
use futures::lock::Mutex;
use std::collections::HashMap;
use std::sync::Arc;

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
            k8s::client::Client::new()
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

    pub async fn get_function(self_: &Arc<FunctionTable>, name: &str) -> Arc<Autoscaler> {
        let mut inner = self_.inner.lock().await;
        match inner.functions.get(name) {
            None => {
                let fm = Autoscaler::new(
                    Arc::downgrade(self_),
                    FunctionManager::new(
                        inner.k8s_client.clone(),
                        inner.http_client.clone(),
                        name.to_string(),
                    )
                    .await,
                );
                inner.functions.insert(name.to_string(), fm.clone());
                return fm;
            }
            Some(value) => {
                return value.clone();
            }
        }
    }
}
