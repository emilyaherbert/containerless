use crate::autoscaler::Autoscaler;
use crate::function_manager::FunctionManager;
use crate::k8s;
use crate::types::*;
use futures::lock::Mutex;
use std::collections::HashMap;
use std::sync::Arc;

struct FunctionTableImpl {
    functions: HashMap<String, Autoscaler>,
    http_client: HttpClient,
    k8s_client: K8sClient,
}

#[derive(Clone)]
pub struct FunctionTable {
    inner: Arc<Mutex<FunctionTableImpl>>,
}

impl FunctionTableImpl {
    async fn new() -> FunctionTableImpl {
        let functions = HashMap::new();
        let k8s_client = Arc::new(
            k8s::client::Client::new()
                .await
                .expect("initializing k8s client"),
        );
        let http_client = Arc::new(hyper::Client::new());
        return FunctionTableImpl {
            functions,
            http_client,
            k8s_client,
        };
    }
}

impl FunctionTable {
    pub async fn new() -> FunctionTable {
        let inner = Arc::new(Mutex::new(FunctionTableImpl::new().await));
        return FunctionTable { inner };
    }

    pub async fn shutdown(&self) -> () {
        let mut inner = self.inner.lock().await;
        for (name, fm) in inner.functions.drain() {
            if let Err(err) = fm.shutdown().await {
                println!("Error shutting down {}: {}", name, err);
            }
        }
    }

    pub async fn get_function(&self, name: &str) -> Autoscaler {
        let mut inner = self.inner.lock().await;
        match inner.functions.get(name) {
            None => {
                let fm = Autoscaler::new(
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
