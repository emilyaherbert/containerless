use crate::util;
use futures::future;
use std::convert::TryInto;
use std::sync::atomic::Ordering::SeqCst;
use std::sync::{atomic, Arc, Mutex};
use std::time::Duration;

#[derive(Clone, Copy)]
struct Measurement {
    num_samples: usize,
    total_time: usize,
}

struct TimeKeeperBuffer {
    measurements: Vec<Measurement>,
    next_index: usize,
    total_time: usize,
    total_samples: usize,
}

struct TimeKeeperData {
    buffer: Mutex<TimeKeeperBuffer>,
    current_time: atomic::AtomicUsize,
    current_samples: atomic::AtomicUsize,
}

#[derive(Clone)]
pub struct TimeKeeper {
    data: Arc<TimeKeeperData>,
}

impl Measurement {
    fn new() -> Measurement {
        return Measurement {
            num_samples: 0,
            total_time: 0,
        };
    }
}

impl TimeKeeperBuffer {
    fn new(window: usize) -> TimeKeeperBuffer {
        let measurements = vec![Measurement::new(); window];
        let next_index = 0;
        let total_time = 0;
        let total_samples = 0;
        TimeKeeperBuffer {
            measurements,
            next_index,
            total_time,
            total_samples,
        }
    }

    fn mean(&self) -> usize {
        if self.total_samples == 0 {
            0
        } else {
            self.total_time / self.total_samples
        }
    }
}

impl TimeKeeperData {
    fn new(window: usize) -> TimeKeeperData {
        let buffer = Mutex::new(TimeKeeperBuffer::new(window));
        let current_time = atomic::AtomicUsize::new(0);
        let current_samples = atomic::AtomicUsize::new(0);
        TimeKeeperData {
            buffer,
            current_time,
            current_samples,
        }
    }
}

impl TimeKeeper {
    pub fn new(window: Duration) -> TimeKeeper {
        let len: usize = window.as_secs().try_into().unwrap();
        let data = Arc::new(TimeKeeperData::new(len));
        let data2 = data.clone();
        tokio::executor::spawn(util::set_interval(Duration::from_secs(1), move |_t| {
            let mut buffer = data.buffer.lock().unwrap();
            let current_time = data.current_time.load(SeqCst);
            let current_samples = data.current_samples.load(SeqCst);
            // Other threads are always adding. We subtract.
            data.current_time.fetch_sub(current_time, SeqCst);
            data.current_samples.fetch_sub(current_samples, SeqCst);
            // Add current measurement to tally
            let i = buffer.next_index;
            let mut next = &mut buffer.measurements[i];
            next.num_samples = current_samples;
            next.total_time = current_time;
            buffer.total_samples += current_samples;
            buffer.total_time += current_time;
            let i = (buffer.next_index + 1) % len;
            buffer.next_index = i;
            // Remove the cleared measurement from tally
            buffer.total_samples -= buffer.measurements[i].num_samples;
            buffer.total_time -= buffer.measurements[i].total_time;
            buffer.measurements[i].num_samples = 0;
            buffer.measurements[i].total_time = 0;
            return future::ok(());
        }));
        TimeKeeper { data: data2 }
    }

    pub fn record_time(&self, duration: Duration) {
        let duration: usize = duration.as_millis().try_into().unwrap();
        self.data.current_time.fetch_add(duration, SeqCst);
        self.data.current_samples.fetch_add(1, SeqCst);
    }

    pub fn mean(&self) -> usize {
        let data = self.data.buffer.lock().unwrap();
        data.mean()
    }
}
