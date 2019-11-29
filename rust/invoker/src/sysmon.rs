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
use std::io::stdout;

pub fn sysmon(config: &InvokerConfig) {
    if OS::detect() != OS::Linux {
        eprintln!("Utilization log requires Linux");
        return;
    }

    let mut utilization_writer : LineWriter<Box<dyn Write + Send>> = LineWriter::new(
        if &config.utilization_log == "stdout" { 
            Box::new(stdout())
        } else {
            Box::new(File::create(&config.utilization_log).unwrap())
        });

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
