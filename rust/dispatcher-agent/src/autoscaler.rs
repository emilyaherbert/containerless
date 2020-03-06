//! An autoscalar for a single serverless function.
//!
//! This autoscalar works by manipulating the number of replicas in a
//! ReplicaSet. The number of replicas is the maximum number of concurrent
//! requests received over the last 60 seconds (bounded by MAX_REPLICAS).
//!
//! Instead exactly tracking the number of concurrent connections over time,
//! it accumulates the total number of connections in windows of time
//! INTERVAL_TIMESPAN_SECONDS. For example, suppose INTERVAL_TIMESPAN_SECONDS
//! is two seconds, and the function processes five connections in one second,
//! and another five in the next second, the number of replicas will be 10, instead
//! of five.
use crate::function_manager::FunctionManager;
use crate::function_table::FunctionTable;
use crate::types::*;
use crate::windowed_max::WindowedMax;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Weak};
use std::time::Duration;
use tokio::task;
use tokio::time::delay_for;

// TODO(arjun): should be an environment variable
const MAX_REPLICAS: usize = 4;
// This could be a parameter.
const NUM_INTERVALS: usize = 12;
// This could be a parameter.
const INTERVAL_TIMESPAN_SECONDS: u64 = 5;

pub struct Autoscaler {
    pending_requests: AtomicUsize,
    max_pending_requests: AtomicUsize,
    is_shutdown: AtomicBool,
    function_manager: Arc<FunctionManager>,
    function_table: Weak<FunctionTable>,
}

async fn update_latency(autoscaler: Arc<Autoscaler>) {
    let mut max_pending_requests_vec = WindowedMax::new(NUM_INTERVALS);

    delay_for(Duration::from_secs(INTERVAL_TIMESPAN_SECONDS)).await;
    let fm = &autoscaler.function_manager;

    loop {
        let current_max_pending = autoscaler.max_pending_requests.swap(0, Ordering::SeqCst);
        max_pending_requests_vec.add(current_max_pending);
        // Calculated number of replicas, bounded above by MAX_REPLICAS
        // above (inclusively).
        let num_replicas = MAX_REPLICAS.min(max_pending_requests_vec.max());
        if num_replicas == 0 {
            if let Some(table) = autoscaler.function_table.upgrade() {
                eprintln!(
                    "Shutting down {} (current_max_pending: {})",
                    fm.name(),
                    current_max_pending
                );
                table.shutdown_function(fm.name()).await;
            }
            return;
        }

        if num_replicas != fm.num_replicas() as usize {
            eprintln!("set_replicas({})", num_replicas);
            if let Err(err) = fm.set_replicas(num_replicas as i32).await {
                eprintln!("Error from set_replicas: {}", err);
            }
        }
        if autoscaler.is_shutdown.load(Ordering::SeqCst) {
            return;
        }
        delay_for(Duration::from_secs(INTERVAL_TIMESPAN_SECONDS)).await;
    }
}

impl Autoscaler {
    pub async fn new(
        function_table: Weak<FunctionTable>,
        k8s: K8sClient,
        client: HttpClient,
        name: String,        
    ) -> Arc<Autoscaler> {
        let function_manager = FunctionManager::new(k8s, client, name).await;
        let pending_requests = AtomicUsize::new(0);
        // Initializing this to 1 is a little hack that prevents immediate shutdown
        let max_pending_requests = AtomicUsize::new(1);
        let is_shutdown = AtomicBool::new(false);
        let autoscaler = Arc::new(Autoscaler {
            pending_requests,
            max_pending_requests,
            is_shutdown,
            function_manager,
            function_table,
        });
        task::spawn(update_latency(autoscaler.clone()));
        return autoscaler;
    }

    pub async fn invoke(
        &self,
        method: http::Method,
        path_and_query: &str,
        body: hyper::Body,
    ) -> Result<Response, hyper::Error> {
        self.pending_requests.fetch_add(1, Ordering::SeqCst);
        let resp = self
            .function_manager
            .invoke(method, path_and_query, body)
            .await;
        let candidate_max_pending = self.pending_requests.fetch_sub(1, Ordering::SeqCst);
        let mut stored_max_pending = self.max_pending_requests.load(Ordering::SeqCst);
        while candidate_max_pending > stored_max_pending {
            let previous = self.max_pending_requests.compare_and_swap(
                stored_max_pending,
                candidate_max_pending,
                Ordering::SeqCst,
            );
            if previous == stored_max_pending {
                return resp;
            }
            stored_max_pending = previous;
        }

        return resp;
    }

    pub async fn shutdown(&self) -> Result<(), kube::Error> {
        self.is_shutdown.store(true, Ordering::SeqCst);
        return self.function_manager.shutdown().await;
    }
}
