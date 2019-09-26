mod config;
mod container_pool;
mod error;
mod mpmc;
mod server;
mod types;

use clap::{App, Arg};
use config::Config;
use futures::future::Future;

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
                .takes_value(true)
                .default_value("3000")
                .help("The listening port number internal to containers"),
        )
        .arg(
            Arg::with_name("container-hostname")
                .takes_value(true)
                .default_value("localhost")
                .help(
                    "The host on which container run. Same as the hostname of DOCKER_HOST, if set.",
                ),
        )
        .arg(
            Arg::with_name("bind-port")
                .takes_value(true)
                .default_value("8080")
                .help("The port number at which decontainer-invoke listens for requests"),
        )
        .get_matches();
    let config = Config {
        container_internal_port: matches
            .value_of("container-internal-port")
            .unwrap()
            .parse()
            .unwrap(),
        container_hostname: matches.value_of("container-hostname").unwrap().to_string(),
        image_name: matches.value_of("image-name").unwrap().to_string(),
        bind_port: matches.value_of("bind-port").unwrap().parse().unwrap(),
    };

    hyper::rt::run(server::serve(config).map_err(|err| {
        println!("Error: {}", err);
        return ();
    }));
}
