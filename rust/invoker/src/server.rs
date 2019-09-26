use crate::config::Config;
use crate::container_pool::ContainerPool;
use crate::error::Error;
use futures::stream::Stream;
use futures::{future, Future};
use hyper::service::service_fn;
use hyper::{Client, Server};
use std::convert::TryInto;
use std::sync::atomic;
use std::sync::Arc;
use std::time::{Duration};

struct State {
    usage_counter: atomic::AtomicI64
}

impl State {
    fn new() -> Arc<State> {
        let usage_counter = atomic::AtomicI64::new(0);
        Arc::new(State { usage_counter })
    }
}

fn monitor_usage(pool: &Arc<ContainerPool>, state: &Arc<State>) {
    let usage_counter = &state.usage_counter;
    // num_containers will fit in i64
    let n: i64 = pool.get_num_containers().try_into().unwrap();
    // Note that all other threads are incrementing usage_counter.
    let usage = usage_counter.load(atomic::Ordering::SeqCst);
    let mut dec_by = 0;
    for _i in 0 .. n {
        if dec_by + 1000 > usage {
            dec_by = usage;
            break;
        }
        dec_by = dec_by + 1000;
    }
    usage_counter.fetch_add(-dec_by, atomic::Ordering::SeqCst);
    let load = usage;
    let remaining_capacity = n * 1000 - load;
    if usage > 500 && remaining_capacity <= 1000 && !pool.is_spawning() {

        tokio::executor::spawn(ContainerPool::create_containers(&pool, 1).map_err(|err| {
            panic!("Error during spawn: {}", err);
          })) ;
        println!("usage = {}, n = {}, load = {}", usage, n, load);
    }

}

pub fn serve(config: Config) -> impl Future<Item = (), Error = Error> {
    let addr = ([0, 0, 0, 0], config.bind_port).into();

    let client = Arc::new(Client::new());
    let config = Arc::new(config);

    let pool = Arc::new(ContainerPool::new(config, client));
    let pool2 = Arc::clone(&pool);

    let state = State::new();
    let state2 = Arc::clone(&state);

    let new_svc = move || {
        let pool2 = Arc::clone(&pool2);
        let state = Arc::clone(&state2);
        service_fn(move |req| {
            let state = Arc::clone(&state);
            ContainerPool::request(&pool2, req).map(move |(resp, duration)| {
                let t: i64 = duration
                    .as_millis()
                    .try_into()
                    // Only occurs if a single invocation takes over 200
                    // million years to complete...
                    .expect("invocation time (in milliseconds) does not fit in i64");
                state.usage_counter.fetch_add(t, atomic::Ordering::SeqCst);
                resp
            })
        })
    };

    // let usage_counter = Arc::clone(&usage_counter2);
    let pool2 = Arc::clone(&pool);

        Future::join(
            tokio::timer::Interval::new_interval(Duration::from_secs(1))
                .map(move |_t| monitor_usage(&pool2, &state))
                .fold((), |(), ()| future::ok(()))
                .map_err(|_err| Error::Unknown),
            Server::bind(&addr).serve(new_svc).from_err(),
        )
        .map(|((), ())| ())
}
