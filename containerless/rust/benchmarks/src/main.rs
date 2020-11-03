mod benchmark_runner;
mod benchmarks;
mod error;

use nix::sys::signal::{self, Signal};
use nix::unistd::Pid;
use shared::net;
use std::convert::TryInto;
use std::time::Duration;
use tokio::process::{Child, Command};
use tokio::signal::unix::{signal, SignalKind};

async fn run_tests() -> Child {
    return Command::new("cargo")
        .args(&["test", "--", "--test-threads=1"])
        .spawn()
        .expect("spawning cargo test");
}

fn child_pid(child: &Child) -> Pid {
    return Pid::from_raw(child.id().try_into().unwrap());
}

#[tokio::main]
async fn main() {
    Command::new("git")
        .args(&[
            "checkout",
            "../dispatcher-agent/src/decontainerized_functions/mod.rs",
        ])
        .spawn()
        .expect("spawning git")
        .await
        .expect("running git");

    let http_client = reqwest::Client::new();
    net::poll_url_no_timeout(
        &http_client,
        "http://localhost/dispatcher/readinessProbe",
        Duration::from_secs(1),
    )
    .await;

    let tests_handle = run_tests().await;
    let tests_pid = child_pid(&tests_handle);

    // Send SIGTERM to "cargo test" when we receive SIGTERM.
    tokio::task::spawn(async move {
        let mut sigterm = signal(SignalKind::terminate()).expect("registering SIGTERM handler");
        sigterm.recv().await;
        signal::kill(tests_pid, Signal::SIGTERM).unwrap();
    });

    // Tests either terminate normally, or with SIGTERM.
    tests_handle.await.unwrap();
}