use std::time::Duration;
use std::thread;
use std::sync::{Arc, Mutex};
use std::convert::TryInto;

#[derive(Clone, Copy)]
struct Measurement {
    num_samples: usize,
    total_time: usize
}

struct TimeKeeperData {
    measurements: Vec<Measurement>,
    next_index: usize,
    total_time: usize,
    total_samples: usize
}


#[derive(Clone)]
pub struct TimeKeeper {
    data: Arc<Mutex<TimeKeeperData>>
}

impl Measurement {
    fn new() -> Measurement {
        return Measurement { num_samples: 0, total_time: 0 };
    }
}

impl TimeKeeperData {
    fn new(window: usize) -> TimeKeeperData {
        let measurements = vec![Measurement::new(); window];
        let next_index = 0;
        let total_time = 0;
        let total_samples = 0;
        TimeKeeperData { measurements, next_index, total_time, total_samples }
    }

    fn mean(&self) -> usize {
        if self.total_samples == 0 { 0 } else { self.total_time / self.total_samples }
    }

}


impl TimeKeeper {

    pub fn new(window: Duration) -> TimeKeeper {
        let len: usize = window.as_secs().try_into().unwrap();
        let data = Arc::new(Mutex::new(
            TimeKeeperData::new(len)));
        let data2 = data.clone();
        thread::spawn(move || {
            loop {
                thread::sleep(Duration::from_secs(1));
                let mut data = data.lock().unwrap();
                // Add current measurement to tally
                let current_measurement = data.measurements[data.next_index];
                data.total_samples += current_measurement.num_samples;
                data.total_time += current_measurement.total_time;
                let i = (data.next_index + 1) % len;
                data.next_index = i;
                // Remove the cleared measurement from tally
                data.total_samples -= data.measurements[i].num_samples;
                data.total_time -= data.measurements[i].total_time;
                data.measurements[i].num_samples = 0;
                data.measurements[i].total_time = 0;
            }
        });
        TimeKeeper { data: data2 }
    }

    pub fn record_time(&self, duration: Duration) {
        let mut data = self.data.lock().unwrap();
        let duration: usize = duration.as_millis().try_into().unwrap();
        let i = data.next_index;
        data.measurements[i].num_samples += 1;
        data.measurements[i].total_time += duration;
    }

    pub fn mean(&self) -> usize {
        let data = self.data.lock().unwrap();
        data.mean()
    }

}

