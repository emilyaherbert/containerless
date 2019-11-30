//! # Invoker
//!
//! `invoker` manages a pool of containers that it uses to execute serverless
//! functions in isolation.
//! 
//! At a high level, the invoker operates by managing requests with a goal of
//! reaching language-level isolation through Rust.
//! * The invoker starts a server with a service function that directs requests
//! to an `IsolationPool`.
//! * The `IsolationPool` organizes the `ContainerPool`, the tracing container,
//! and `Decontainer`.
//! * Typically we want to be tracing. In this case, the tracing container is
//! prioritized and requests are attempted to be sent there, falling back on
//! sending them to the `ContainerPool`. The tracing container executes a
//! version of the serverless function that is instrumented with runtime
//! statements to build a trace. Once some number of requests have been
//! completed, the trace is extracted and compiled to Rust code.
//! * The `ContainerPool` handles its own container starting, stopping, and
//! management. Requests sent there are then sent to either an available
//! container or a new container is created (within a certain limit) and made
//! available.
//! * Once ready, requests are executed in Rust using `trace_runtime::Decontainer`.

pub mod container_handle;
pub mod container_pool;
pub mod isolation_pool;
pub mod error;
pub mod mock;
pub mod mpmc;
pub mod server;
pub mod sysmon;
pub mod time_keeper;
pub mod trace_runtime;
pub mod types;
pub mod util;

use clap::{App, Arg};
use futures::future::{self, Future};
use futures::stream::Stream;
use shared::config::InvokerConfig;
use std::io::{self, Read};
use std::sync::Arc;

pub fn main(containerless: Option<trace_runtime::Containerless>) {
    eprintln!("Starting Decontainerizer");
    let matches = App::new("decontainerizer-invoker")
        .arg(
            Arg::with_name("testing")
                .long("--testing")
                .help("Set to run in test mode (input on stdin)"),
        )
        .arg(
            Arg::with_name("config")
                .long("--config")
                .takes_value(true)
                .help("Configuration JSON object (as a string)"),
        )
        .get_matches();

    if matches.is_present("testing") {
        // Note that we are silently ignoring all the other options.
        return testing_main(containerless.expect("need decontainerized function for testing"));
    }

    let config = Arc::new(InvokerConfig::from_string(
        matches.value_of("config").unwrap(),
    ));

    hyper::rt::run(future::lazy(move || {
        sysmon::sysmon(&config);
        server::serve(config.clone(), containerless)
            .map_err(|err| {
                println!("Error: {}", err);
                return ();
            })
            .map(move |()| {
                println!("Graceful shutdown");
                // TODO(emily): Fix graceful shutdown
                /*
                if config.kill_parent {
                    kill(Pid::parent(), Signal::SIGUSR1).expect("Could not signal parent process");
                }
                */
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
    let requests = mock::Request::from_string_vec(&raw_input);
    for request in requests.into_iter() {
        let https = hyper_rustls::HttpsConnector::new(4);
        let client = Arc::new(hyper::Client::builder().build(https));
        tokio::run(
            Decontainer::new_from(containerless, client, &request.path, request.body)
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
