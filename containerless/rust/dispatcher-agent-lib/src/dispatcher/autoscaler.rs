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
use super::function_table::FunctionTable;
use super::types::*;
use super::windowed_max::WindowedMax;
use std::sync::Arc;
use std::time::Duration;
use tokio::task;
use tokio::time::delay_for;

// TODO(arjun): should be an environment variable
const MAX_REPLICAS: usize = 4;
// This could be a parameter.
const NUM_INTERVALS: usize = 3;
// This could be a parameter.
const INTERVAL_TIMESPAN_SECONDS: u64 = 5;

pub struct Autoscaler {
    pending_requests: AtomicUsize,
    max_pending_requests: AtomicUsize,
    k8s_client: K8sClient,
    replica_set: String,
    name: String,
    terminated: AtomicBool,
}

impl Autoscaler {
    async fn set_replicas(&self, n: i32) -> Result<(), kube::Error> {
        use k8s::builder::*;
        let rs = ReplicaSetBuilder::new()
            .metadata(ObjectMetaBuilder::new().name(&self.replica_set).build())
            .spec(ReplicaSetSpecBuilder::new().replicas(n).build())
            .build();
        info!(target: "dispatcher", "setting replicas = {} for rs/{}", n, &self.replica_set);
        return self.k8s_client.patch_replica_set(rs).await;
    }

    async fn update_latency(
        autoscaler: Arc<Autoscaler>, function_table: Weak<FunctionTable>, init_num_replicas: i32,
    ) {
        let mut max_pending_requests_vec = WindowedMax::new(NUM_INTERVALS);

        delay_for(Duration::from_secs(INTERVAL_TIMESPAN_SECONDS)).await;
        let mut last_num_replicas = init_num_replicas;
        loop {
            if autoscaler.terminated.load(SeqCst) {
                return;
            }
            let current_max_pending = autoscaler.max_pending_requests.swap(0, SeqCst);
            max_pending_requests_vec.add(current_max_pending);
            // Calculated number of replicas, bounded above by MAX_REPLICAS
            // above (inclusively).
            let num_replicas = MAX_REPLICAS.min(max_pending_requests_vec.max()) as i32;
            if num_replicas == 0 {
                let ft = function_table.upgrade().expect("FunctionTable is nil");
                FunctionTable::shutdown(ft, &autoscaler.name).await;
                return;
            }

            if num_replicas != last_num_replicas {
                if let Err(err) = autoscaler.set_replicas(num_replicas).await {
                    eprintln!("Error from set_replicas: {}", err);
                }
                last_num_replicas = num_replicas;
            }
            delay_for(Duration::from_secs(INTERVAL_TIMESPAN_SECONDS)).await;
        }
    }

    pub fn new(
        k8s_client: K8sClient, replica_set: String, function_table: Weak<FunctionTable>,
        init_num_replicas: i32, name: String,
    ) -> Arc<Autoscaler> {
        let max_pending_requests = AtomicUsize::new(1);
        let pending_requests = AtomicUsize::new(0);
        let terminated = AtomicBool::new(false);
        let autoscaler = Autoscaler {
            pending_requests,
            max_pending_requests,
            k8s_client,
            replica_set,
            name,
            terminated,
        };
        let autoscaler = Arc::new(autoscaler);

        task::spawn(Autoscaler::update_latency(
            autoscaler.clone(),
            function_table,
            init_num_replicas,
        ));
        return autoscaler;
    }

    pub fn terminate(&self) {
        self.terminated.store(true, SeqCst);
    }

    pub fn recv_req(&self) {
        self.pending_requests.fetch_add(1, SeqCst);
    }

    pub fn recv_resp(&self) {
        let candidate_max_pending = self.pending_requests.fetch_sub(1, SeqCst);
        let mut stored_max_pending = self.max_pending_requests.load(SeqCst);
        while candidate_max_pending > stored_max_pending {
            let previous = self.max_pending_requests.compare_and_swap(
                stored_max_pending,
                candidate_max_pending,
                SeqCst,
            );
            if previous == stored_max_pending {
                return;
            }
            stored_max_pending = previous;
        }
    }
}
