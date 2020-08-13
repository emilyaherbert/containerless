#[macro_use]
extern crate log;

mod test_runner;
mod tests;

use machine_ip;
use nix::sys::signal::{self, Signal};
use nix::unistd::Pid;
use shared::net;
use std::convert::TryInto;
use std::time::Duration;
use tokio::process::{Child, Command};
use tokio::signal::unix::{signal, SignalKind};

async fn start_controller() -> Child {
    let my_ip = machine_ip::get().expect("getting IP address");
    return Command::new("cargo")
        .arg("run")
        .current_dir("../controller-agent") // TODO(arjun): fix
        .env("LOG_LEVEL", "info")
        .env("LOG_RSYSLOG_ADDR", format!("{}:514", my_ip))
        .spawn()
        .expect("spawning controller");
}

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
    let controller_handle = start_controller().await;
    let controller_pid = child_pid(&controller_handle);
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

    // Send SIGTERM to controller. Note that we reach this line even if this
    // process receives SIGTERM.
    signal::kill(controller_pid, Signal::SIGTERM).unwrap();
}
