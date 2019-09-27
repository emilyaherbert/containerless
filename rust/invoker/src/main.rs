mod config;
mod container_pool;
mod error;
mod mpmc;
mod server;
mod time_keeper;
mod types;
mod util;

use clap::{App, Arg};
use config::Config;
use futures::future::{self, Future};

fn main() {
    println!("Starting Decontainerizer");
    let matches = App::new("decontainerizer-invoker")
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
        .get_matches();
    let config = Config {
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
            .value_of("max-container-lifespan")
            .unwrap()
            .parse()
            .unwrap(),
        max_containers: matches.value_of("max-containers").unwrap().parse().unwrap(),
        container_hostname: matches.value_of("container-hostname").unwrap().to_string(),
        image_name: matches.value_of("image-name").unwrap().to_string(),
        bind_port: matches.value_of("bind-port").unwrap().parse().unwrap(),
    };

    hyper::rt::run(future::lazy(|| {
        server::serve(config).map_err(|err| {
            println!("Error: {}", err);
            return ();
        })
    }));
}
