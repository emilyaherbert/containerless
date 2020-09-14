#![cfg(test)]
use super::benchmark_runner;

#[test]
fn sanity_check() {
    let code =r#"
        let containerless = require("containerless");
        containerless.listen(function(req) {
            containerless.respond("Hello, world!");
        });"#;

    let wrk_options = benchmark_runner::WrkOptions {
        connections: 10,
        duration: 10,
        threads: 4
    };

    let result = benchmark_runner::run_benchmark("sanity", code, "http://google.com", wrk_options);

    assert_eq!(result, "Done!".to_string());
}

#[test]
fn hello_world() {
    let code =r#"
        let containerless = require("containerless");
        containerless.listen(function(req) {
            containerless.respond("Hello, world!");
        });"#;

    let wrk_options = benchmark_runner::WrkOptions {
        connections: 10,
        duration: 60,
        threads: 4
    };

    let result = benchmark_runner::run_benchmark("helloworld", code, "http://localhost/dispatcher/helloworld", wrk_options);

    assert_eq!(result, "Done!".to_string());
}