use shared::config::InvokerConfig;
use shared::OS;
use std::convert::TryInto;
use std::fs::File;
use std::io::LineWriter;
use std::io::Write;
use std::thread;
use std::time::Duration;
use systemstat::{Platform, System};
use super::util;

pub fn sysmon(config: &InvokerConfig) {
    if OS::detect() != OS::Linux {
        eprintln!("Utilization log requires Linux");
        return;
    }

    let utilization_log =
        File::create(&config.utilization_log).expect("could not create utilization log file");
    let mut utilization_writer = LineWriter::new(utilization_log);
            utilization_writer
                .write_fmt(format_args!("Time,CPU,Memory"))
                .unwrap();

    // NOTE(arjun): trying to do this with Tokio and futures_locks runs into
    // Send + 'static issues that I don't feel like debugging.
    thread::spawn(move || {
        let sys = System::new();
        let mut cpu_load = sys.cpu_load_aggregate().unwrap();
        loop {
            thread::sleep(Duration::from_millis(100));
            let load = cpu_load.done().expect("could not read CPU load");
            let mem = sys.memory().expect("could not read memory");
            let mem_total: u64 = mem.total.as_usize().try_into().unwrap();
            let mem_free: u64 = mem.free.as_usize().try_into().unwrap();
            let mem_total = mem_total as f64;
            let mem_free = mem_free as f64;
            let mem_load = (mem_total - mem_free) / mem_total;
            utilization_writer
                .write_fmt(format_args!("{},{},{}\n", util::unix_epoch_ms(), 1.0 - load.idle, mem_load))
                .unwrap();
            cpu_load = sys.cpu_load_aggregate().unwrap();
        }
    });
}
