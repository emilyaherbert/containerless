use futures::{Future, Stream}; // 0.1.25
use hyper::Client; // 0.12.23
use tokio; // 0.1.15
use std::time::{Duration, Instant};
use futures::future::{lazy};
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();

    //let num = ((duration * 1000) / interval) as

    let start = Instant::now();
    let task = tokio::timer::Interval::new_interval(Duration::from_millis(5))
        .take(2600)
        .for_each(move |_| {
            let client = Client::new();
            tokio::spawn(lazy(move || {
                let uri = "http://localhost:8080/hello".parse().unwrap();
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
        .map_err(|e| panic!("Something went wrong!"));

    tokio::run(task);
}