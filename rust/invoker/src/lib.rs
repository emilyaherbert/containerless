mod container_handle;
mod container_pool;
mod decontainer_pool;
mod error;
mod mpmc;
mod server;
mod sysmon;
mod time_keeper;
pub mod trace_runtime;
mod types;
mod util;

use shared::config::InvokerConfig;
use clap::{App, Arg};
use futures::future::{self, Future};
use futures::stream::Stream;
use std::io::{self, Read};
use std::sync::Arc;
use nix::sys::signal::{Signal, kill};
use nix::unistd::Pid;

pub fn main(containerless: Option<trace_runtime::Containerless>) {
    eprintln!("Starting Decontainerizer");
    let matches = App::new("decontainerizer-invoker")
        .arg(Arg::with_name("testing")
            .long("--testing")
            .help("Set to run in test mode (input on stdin)"))
        .arg(Arg::with_name("config")
            .long("--config")
            .takes_value(true)
            .help("Configuration JSON object (as a string)"))
        .get_matches();

    if matches.is_present("testing") {
        // Note that we are silently ignoring all the other options.
        return testing_main(containerless.expect("need decontainerized function for testing"));
    }

    let config = Arc::new(InvokerConfig::from_string(matches.value_of("config").unwrap()));

    hyper::rt::run(future::lazy(move || {
        sysmon::sysmon(&config);
        server::serve(config.clone(), containerless).map_err(|err| {
            println!("Error: {}", err);
            return ();
        }).map(move |()| {
            println!("Graceful shutdown");
            // TODO(emily): Fix graceful shutdown
            if config.kill_parent {
                kill(Pid::parent(), Signal::SIGUSR1);
            }
            std::process::exit(0)
        })
    }));
}

fn testing_main(containerless: trace_runtime::Containerless) {
    use trace_runtime::Decontainer;
    let mut raw_input = String::new();
    io::stdin()
        .read_to_string(&mut raw_input)
        .expect("could not read stdin");
    let lines = raw_input.split_terminator('\n');
    for line in lines {
        tokio::run(
            Decontainer::new_from(containerless, line)
                .map_err(|err| {
                    eprintln!("Error: {:?}", err);
                    std::process::exit(1);
                })
                .and_then(|resp| {
                    resp.into_body().concat2().map_err(|_err| ()).map(|chunk| {
                        let v = chunk.to_vec();
                        println!("{}", String::from_utf8_lossy(&v).to_string())
                    })
                }),
        );
    }
}
