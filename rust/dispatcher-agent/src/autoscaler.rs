use tokio::task;
use crate::function_manager::FunctionManager;
use crate::types::*;
use std::sync::atomic::{AtomicUsize, AtomicI64, Ordering};
use std::sync::Arc;
use std::time::{Instant, Duration};
use tokio::time::delay_for;
use crate::fixed_window::FixedWindow;
use std::convert::TryFrom;

const NUM_INTERVALS: usize = 10;
const INTERVAL_TIMESPAN_SECONDS: u64 = 5;
const DELTA: i64 = 50;
const K: usize = 10;

enum State {
    Steady(usize),
    ScaledUp,
    ScaledDown
}

struct AutoscalerInner {
    num_reqs_in_current_interval: AtomicI64,
    total_req_time_in_current_interval: AtomicI64,
    latency: AtomicUsize,
    function_manager: FunctionManager
}

#[derive(Clone)]
pub struct Autoscaler {
    inner: Arc<AutoscalerInner>
}

fn calc_latency(num_reqs: &FixedWindow<i64>, total_time: &FixedWindow<i64>) -> i64 {
    if num_reqs.sum() == 0 {
        return 0;
    } else {
        eprintln!("num_requests = {}, total_time = {}", num_reqs.sum(), total_time.sum());
        return total_time.sum() / num_reqs.sum();
    }
}

async fn update_latency(autoscaler: Autoscaler) {
    let mut num_reqs = FixedWindow::<i64>::new(0, NUM_INTERVALS);
    let mut total_time = FixedWindow::<i64>::new(0, NUM_INTERVALS);

    delay_for(Duration::from_secs(INTERVAL_TIMESPAN_SECONDS)).await;
    let mut state = State::Steady(0);
    let mut latency = calc_latency(&num_reqs, &total_time);

    loop {
        delay_for(Duration::from_secs(INTERVAL_TIMESPAN_SECONDS)).await;

        // Measure latency
        num_reqs.add(autoscaler.inner.num_reqs_in_current_interval.swap(0, Ordering::SeqCst));
        total_time.add(autoscaler.inner.total_req_time_in_current_interval.swap(0, Ordering::SeqCst));
        num_reqs.move_window();
        total_time.move_window();
        let new_latency = calc_latency(&num_reqs, &total_time);
        let fm = &autoscaler.inner.function_manager;
        println!("latency = {}, new_latency = {}", latency, new_latency);
        match state {
            State::Steady(n) => {
                if new_latency - latency > DELTA {
                    // Latency has increased, thus create new nodes.
                    state = State::ScaledUp;
                    latency = new_latency;
                    let fm = &autoscaler.inner.function_manager;
                    fm.set_replicas(fm.num_replicas() + 1).await;
                    // create new nodes
                }
                else if n < K {
                    // Still stable
                    state = State::Steady(n + 1);
                    latency = new_latency;
                }
                else {
                    // We have been steady for some time. Try to scale down.
                    state = State::ScaledDown;
                    latency = new_latency;
                }
            },
            State::ScaledUp => {
                if latency - new_latency > DELTA {
                    // The last Scaleup action decreased latency, so try it
                    // again
                    latency = new_latency;
                    // create new nodes
                }
                else {
                    // The last ScaleUp had minimal effect on latency.
                    state = State::Steady(0);
                }
            }
            State::ScaledDown => {
                let change = (new_latency - latency).abs();
                if change < DELTA {
                    // The last ScaleDown did not adversely affect latency,
                    // so try it again!
                    // TODO: shutdown a node
                }
                else {
                    state = State::Steady(0);
                }
            }
        }
    }
}

impl AutoscalerInner {

    pub fn new(function_manager: FunctionManager) -> AutoscalerInner {
        let num_reqs_in_current_interval = AtomicI64::new(0);
        let total_req_time_in_current_interval = AtomicI64::new(0);
        let latency = AtomicUsize::new(0);
        return AutoscalerInner {
            function_manager,
            latency,
            num_reqs_in_current_interval,
            total_req_time_in_current_interval
        };
    }

    pub fn measure(&self, duration_ms: i64) {
        self.num_reqs_in_current_interval.fetch_add(1, Ordering::SeqCst);
        self.total_req_time_in_current_interval.fetch_add(duration_ms, Ordering::SeqCst);
    }

    pub async fn invoke(
        &self,
        method: http::Method,
        path_and_query: &str,
        body: hyper::Body,
    ) -> Result<Response, hyper::Error> {
        let start_time = Instant::now();
        let resp = self.function_manager.invoke(method, path_and_query, body).await;
        if let Ok(resp) = resp {
            let body = hyper::body::to_bytes(resp.into_body()).await?;
            if let Ok(t) = TryFrom::try_from(start_time.elapsed().as_millis()) {
                self.measure(t);
                return Ok(hyper::Response::builder().body(hyper::Body::from(body)).unwrap());
            }
            panic!("invocation time (milliseconds) did not fit in i64");
        }
        return resp;
    }

    pub async fn shutdown(&self) -> Result<(), kube::Error> {
        // TODO(arjun) Terminate the autoscaling task
        return self.function_manager.shutdown().await;
    }

}

impl Autoscaler {

    pub fn new(function_manager: FunctionManager) -> Self {
        let inner = Arc::new(AutoscalerInner::new(function_manager));
        let autoscaler = Autoscaler { inner };
        task::spawn(update_latency(autoscaler.clone()));
        return autoscaler;
    }
    
    pub fn measure(&self, duration_ms: i64) {
        self.inner.measure(duration_ms);
    }

    pub async fn shutdown(&self) -> Result<(), kube::Error> {
        // TODO(arjun) Terminate the autoscaling task
        return self.inner.shutdown().await;
    }

    pub async fn invoke(
        &self,
        method: http::Method,
        path_and_query: &str,
        body: hyper::Body,
    ) -> Result<Response, hyper::Error> {
        return self.inner.invoke(method, path_and_query, body).await;
    }


}