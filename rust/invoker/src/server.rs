use crate::config::Config;
use crate::error::Error;
use futures::stream::Stream;
use futures::{future, Future};
use hyper::service::service_fn;
use hyper::{Client, Server};
use std::sync::Arc;
use std::sync::atomic::AtomicI64;
use std::sync::atomic;
use std::time::{Instant,Duration};
use std::convert::TryInto;
use crate::container_pool::ContainerPool;

fn monitor_usage(pool: &ContainerPool,
    usage_counter: &atomic::AtomicI64) {
    // num_containers will fit in i64
    let n: i64 = pool.get_num_containers().try_into().unwrap();
    // Note that all other threads are incrementing usage_counter.
    let usage = usage_counter.load(atomic::Ordering::SeqCst);
    let dec_by = (1000 * n).min(usage);
    usage_counter.fetch_add(-dec_by, atomic::Ordering::SeqCst);
    if usage - 1000 > 1000 * n {
        println!("Usage: {}", usage);
    }

}

pub fn serve(config: Config) -> impl Future<Item = (), Error = Error> {
    let addr = ([0, 0, 0, 0], config.bind_port).into();

    let client = Arc::new(Client::new());
    let config = Arc::new(config);

    let pool = Arc::new(ContainerPool::new(config, client));
    let pool2 = Arc::clone(&pool);

    let usage_counter = Arc::new(AtomicI64::new(0));
    let usage_counter2 = Arc::clone(&usage_counter);

    let new_svc = move || {
        let pool2 = Arc::clone(&pool2);
        let usage_counter = Arc::clone(&usage_counter);
        service_fn(move |req| {
            let start = Instant::now();
            let usage_counter = Arc::clone(&usage_counter);
            pool2.request(req)
            .map(move |resp| {
                let t: i64 = start.elapsed().as_millis().try_into()
                    // Only occurs if a single invocation takes over 200
                    // million years to complete...
                    .expect("invocation time (in milliseconds) does not fit in i64");
                usage_counter.fetch_add(t, atomic::Ordering::SeqCst);
                resp
            })
        })
    };

    let usage_counter = Arc::clone(&usage_counter2);
    let pool2 = Arc::clone(&pool);
    pool.create_containers(5).and_then(move |()| {
        Future::join(
            tokio::timer::Interval::new_interval(Duration::from_secs(1))
                .map(move |_t| monitor_usage(&pool2, &usage_counter))
                .fold((), |(), ()| future::ok(()))
                .map_err(|_err| Error::Unknown),
            Server::bind(&addr).serve(new_svc).from_err(),
        )
        .map(|((), ())| ())
    })
}
