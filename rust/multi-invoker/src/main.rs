use clap::{App, Arg};
use duct::cmd;
use forwarding::Forwarding;
use shared::config::MultiInvokerConfig;
use signal_hook::iterator::Signals;

mod forwarding;

pub fn main() {
    let matches = App::new("decontainerizer")
        .arg(
            Arg::with_name("config")
                .long("--config")
                .takes_value(true)
                .help("Configuration JSON object (as a string)"),
        )
        .get_matches();

    let config = MultiInvokerConfig::from_string(matches.value_of("config").unwrap());

    let config_a = config.config_a.to_string();
    let config_b = config.config_b.to_string();

    let signals = Signals::new(&[signal_hook::SIGUSR1, signal_hook::SIGINT])
        .expect("failed to register signal handlers");
    let mut use_config_b = false;

    let mut forwarding = Forwarding::new(config.bind_port, config.config_a.bind_port);
    let mut child_handle = cmd!("cargo", "run", "--", "--config", &config_a)
        .dir("../containerless-scaffold")
        .start()
        .unwrap();

    for signal in signals.forever() {
        match signal {
            signal_hook::SIGINT => {
                child_handle.kill().expect("Could not kill child");
                break;
            }
            signal_hook::SIGUSR1 => {
                println!("Starting new invoker as a child processes.");
                use_config_b = !use_config_b;
                let config_str = if use_config_b { &config_b } else { &config_a };
                child_handle = cmd!("cargo", "run", "--release", "--", "--config", config_str)
                    .dir("../containerless-scaffold")
                    .start()
                    .expect("Could not start child process");
                forwarding.change_destination(if use_config_b {
                    config.config_b.bind_port
                } else {
                    config.config_a.bind_port
                });
            }
            _ => panic!("Received unexpected signal"),
        }
    }
    println!("Terminated multi-invoker");
}
