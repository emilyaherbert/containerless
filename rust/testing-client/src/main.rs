use futures::{
    Future,
    future,
    Stream,
    future::ok
};
use hyper::Client;
use hyper::Request;
use tokio;
use std::{
    time::{
        Duration,
        Instant
    }
};
use futures::future::{lazy};
use rand::Rng;
use time;

fn main() {
    let authority = "10.200.0.25:8080";
    let duration = 60;
    let req_per_sec = 500;

    let path_pool = vec![
        "/login",
        "/logmein"
    ];

    let body_pool = vec![
        "{ \"username\": \"hacker\" }",
        "{ \"username\": \"javascript\", \"password\": \"rust\" }",
        "{ \"username\": \"timmy\", \"password\": \"password\" }"
    ];

    let stop = Duration::from_secs(duration);
    let start = Instant::now();
    let task = tokio::timer::Interval::new_interval(Duration::from_secs_f64(1.0 / (req_per_sec as f64)))
        .take_while(move |_| ok(start.elapsed() < stop))
        .for_each(move |_| {
            let mut rng = rand::thread_rng();
            let request = Request::post(
                hyper::Uri::builder()
                    .scheme("http")
                    .authority(authority)
                    .path_and_query(path_pool[rng.gen_range(0, path_pool.len())])
                    .build()
                    .unwrap()
                )
                .header("Content-Type", "application/json")
                .body(hyper::Body::from(body_pool[rng.gen_range(0, body_pool.len())]))
                .expect("Could not create request.");
            tokio::spawn(lazy(move || {
                let client = Client::new();
                let now = Instant::now();
                // hacky but idc
                if start.elapsed() < stop {
                    future::Either::A(client.request(request)
                        .and_then(move |resp| {
                            // For official testing
                            let t = time::get_time();
                            println!("{:?},{:?}", t.sec + (t.nsec as i64 / 1000), now.elapsed().as_micros());
                            println!("{:?}", start.elapsed().as_secs());
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
                        .map_err(|e| panic!("{:?}", e)))
                } else {
                    future::Either::B(ok(())
                        .and_then(|_| {
                            //println!("Some slow bois returning.");
                            Ok(())
                        }
                    ))
                }
            }));
            Ok(())
        })
        .map_err(|e| panic!("{:?}", e));

    tokio::run(task);
}