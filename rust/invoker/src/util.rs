use std::time::{Instant, Duration, SystemTime, UNIX_EPOCH};
use tokio::timer::Interval;
use futures::{Future, Stream};

/// Seconds since Jan 1, 1970. This takes more than 1 line in Rust.
pub fn unix_epoch_secs() -> u64 {
    let start = SystemTime::now();
    let since_the_epoch = start
        .duration_since(UNIX_EPOCH)
        .expect("current time is earlier than Jan 1, 1970");
    since_the_epoch.as_secs()
}

/// Running a function periodically with Tokio is a bit convoluted, since we
/// get a stream that needs to turn into a future. Example use:
///
/// tokio::executor::spawn(
///   util::set_interval(Duration::from_secs(1), move |t| { ... })
///   .map_err(|err| println!("Error = {}", err)));
pub fn set_interval<F,Fut,E>(duration: Duration, f: F)
  -> impl Future<Item = (), Error = E> where
  Fut : Future<Item = (), Error = E>,
  F: Fn(Instant) -> Fut {
    Interval::new_interval(duration)
        .map_err(|err| panic!("Tokio timer error: {}", err))
        .map(f)
        .fold((), |(), f| f)
}
