use futures::{
    Future,
    Stream,
    future::ok
};
use hyper::Client;
use tokio;
use std::time::{Duration, Instant};
use futures::future::{lazy};
use clap::{App, Arg};

mod config;
use config::TestConfig;

fn main() {
    let matches = App::new("testing-client")
        .arg(
            Arg::with_name("config")
                .long("--config")
                .takes_value(true)
                .help("Configuration JSON object (as a string)"),
        )
        .get_matches();
    let config = TestConfig::from_string(matches.value_of("config").unwrap());

    let stop = Duration::from_secs(config.duration);
    let start = Instant::now();
    let task = tokio::timer::Interval::new_interval(Duration::from_millis(config.rate))
        .take_while(move |_| ok(start.elapsed() < stop))
        .for_each(move |_| {
            let config = config.clone();
            tokio::spawn(lazy(move || {
                let client = Client::new();
                let uri = config.url.parse().unwrap();
                let now = Instant::now();
                client.get(uri)
                    .and_then(move |_| {
                        println!("{:?},{:?}", now.elapsed().as_millis(), start.elapsed().as_millis());
                        Ok(())
                    })
                    .map_err(|e| panic!("{:?}", e))
            }));
            Ok(())
        })
        .map_err(|e| panic!("{:?}", e));

    tokio::run(task);
}