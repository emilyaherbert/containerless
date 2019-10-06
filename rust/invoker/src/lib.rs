mod config;
mod container_pool;
mod error;
mod mpmc;
mod server;
mod sysmon;
mod time_keeper;
pub mod trace_runtime;
mod types;
mod util;

use std::io::{self, Read};
use clap::{App, Arg};
use config::Config;
use futures::future::{self, Future};
use futures::stream::Stream;

pub fn main(containerless: Option<trace_runtime::Containerless>) {
    eprintln!("Starting Decontainerizer");
    let matches = App::new("decontainerizer-invoker")
        .arg(Arg::with_name("testing")
            .long("--testing")
            .help("Set to run in test mode (input on stdin)"))
        .arg(
            Arg::with_name("image-name")
                .takes_value(true)
                .required(true)
                .help("The name of the Docker image that contains the container to run"),
        )
        .arg(
            Arg::with_name("container-internal-port")
                .long("--container-internal-port")
                .takes_value(true)
                .default_value("3000")
                .help("The listening port number internal to containers"),
        )
        .arg(
            Arg::with_name("container-hostname")
                .long("--container-hostname")
                .takes_value(true)
                .default_value("localhost")
                .help(
                    "The host on which container run. Same as the hostname of DOCKER_HOST, if set.",
                ),
        )
        .arg(
            Arg::with_name("bind-port")
                .long("--bind-port")
                .takes_value(true)
                .default_value("8080")
                .help("The port number at which decontainer-invoke listens for requests"),
        )
        .arg(
            Arg::with_name("max-containers")
                .long("--max-containers")
                .takes_value(true)
                .default_value("4")
                .help("The maximum number of containers to run in parallel"),
        )
        .arg(
            Arg::with_name("max-container-buffer-delay")
                .long("--max-container-buffer-delay")
                .takes_value(true)
                .default_value("100")
                .help("The maximum time (ms) a request should spend in the buffer. Containers are shutdown if the mean time in the buffer is lower than this value.")
        )
        .arg(
            Arg::with_name("min-container-lifespan")
                .long("--min-container-lifespan")
                .takes_value(true)
                .default_value("10")
                .help("The minimum lifespan (s) of a container")
        )
        .arg(
            Arg::with_name("cpus")
                .long("--cpus")
                .takes_value(true)
                .default_value("1.0")
                .help("CPUs allocated per container"))
        .arg(
            Arg::with_name("memory")
                .long("--memory")
                .takes_value(true)
                .default_value("512MB")
                .help("Memory allocated per container"))
        .arg(
            Arg::with_name("utilization-log")
            .long("--utilization-log")
            .takes_value(true)
            .default_value("utilization.log")
            .help("Log of CPU and memory utilization"))
        .get_matches();

    if matches.is_present("testing") {
        // Note that we are silently ignoring all the other options.
        return testing_main(containerless.expect("need decontainerized function for testing"));
    }

    let config = Config {
        utilization_log: matches.value_of("utilization-log").unwrap().to_string(),
        memory: matches.value_of("memory").unwrap().to_string(),
        cpus: matches.value_of("cpus").unwrap().to_string(),
        container_internal_port: matches
            .value_of("container-internal-port")
            .unwrap()
            .parse()
            .unwrap(),
        max_container_buffer_delay: matches
            .value_of("max-container-buffer-delay")
            .unwrap()
            .parse()
            .unwrap(),
        min_container_lifespan: matches
            .value_of("min-container-lifespan")
            .unwrap()
            .parse()
            .unwrap(),
        max_containers: matches.value_of("max-containers").unwrap().parse().unwrap(),
        container_hostname: matches.value_of("container-hostname").unwrap().to_string(),
        image_name: matches.value_of("image-name").unwrap().to_string(),
        bind_port: matches.value_of("bind-port").unwrap().parse().unwrap(),
        containerless: containerless,
    };

    hyper::rt::run(future::lazy(|| {
        sysmon::sysmon(&config);
        server::serve(config).map_err(|err| {
            println!("Error: {}", err);
            return ();
        })
    }));
}

fn testing_main(containerless: trace_runtime::Containerless) {
    use trace_runtime::Decontainer;
    let mut raw_input = String::new();
    io::stdin().read_to_string(&mut raw_input).expect("could not read stdin");
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
            }));
    }
}