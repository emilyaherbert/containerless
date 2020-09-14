#![cfg(test)]
use super::benchmark_runner::run_benchmark;
use crate::benchmark_runner::WrkOptions;

#[test]
fn sanity_check() {
    let wrk_options = WrkOptions {
        connections: 10,
        duration: 10,
        threads: 4
    };

    let result = run_benchmark("sanity", "http://google.com", wrk_options);

    assert_eq!(result, "Done!".to_string());
}