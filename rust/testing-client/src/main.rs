use futures::{
    Future,
    Stream,
    future::ok
};
use hyper::Client;
use tokio;
use std::{
    time::{
        Duration,
        Instant
    },
    fs::File,
    io::Read
};
use futures::future::{lazy};
use clap::{App, Arg};

mod config;
use config::TestConfig;

fn main() {
    let matches = App::new("testing-client")
        .arg(Arg::with_name("output")
            .long("--output")
            .help("Prints output from request."))
        .arg(
            Arg::with_name("config")
                .long("--config")
                .takes_value(true)
                .help("Configuration JSON file."),
        )
        .get_matches();
    let mut config_file = File::open(matches.value_of("config").unwrap()).unwrap();
    let mut data = String::new();
    config_file.read_to_string(&mut data).unwrap();
    let config = TestConfig::from_string(&data);

    let stop = Duration::from_secs(config.duration);
    let start = Instant::now();
    let task = tokio::timer::Interval::new_interval(Duration::from_millis(config.rate))
        .take_while(move |_| ok(start.elapsed() < stop))
        .for_each(move |_| {
            let request = config.request.clone();
            tokio::spawn(lazy(move || {
                let client = Client::new();
                let uri = request.url.parse().unwrap();
                let now = Instant::now();
                client.get(uri)
                    .and_then(move |resp| {
                        // For official testing
                        println!("{:?},{:?}", now.elapsed().as_millis(), start.elapsed().as_millis());
                        Ok(())
    
                        // // For debugging, making sure output is correct
                        // resp.into_body().concat2().map_err(|_err| ()).map(|chunk| {
                        //     let v = chunk.to_vec();
                        //     String::from_utf8_lossy(&v).to_string()
                        // }).and_then(move |s| {
                        //     println!("{:?}", s);
                        //     Ok(())
                        // }).map_err(|e| panic!("{:?}", e))
                    })
                    .map_err(|e| panic!("{:?}", e))
            }));
            Ok(())
        })
        .map_err(|e| panic!("{:?}", e));

    tokio::run(task);
}