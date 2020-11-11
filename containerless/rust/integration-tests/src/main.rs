mod error;
mod test_runner;
mod tests;

use nix::sys::signal::{self, Signal};
use nix::unistd::Pid;
use std::convert::TryInto;
use tokio::process::{Child, Command};
use tokio::signal::unix::{signal, SignalKind};

async fn run_tests() -> Child {
    // One test thread is needed for these to be reasonable unit tests. Without it, tests may
    // interfere with each other, since the Dispatcher is very stateful.
    // The --nocapture option lets us see error messages faster.
    return Command::new("cargo")
        .args(&["test", "--", "--test-threads=1", "--nocapture"])
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

    let tests_handle = run_tests().await;
    let tests_pid = child_pid(&tests_handle);

    // Send SIGTERM to "cargo test" when we receive SIGTERM.
    tokio::task::spawn(async move {
        let mut sigterm = signal(SignalKind::terminate()).expect("registering SIGTERM handler");
        sigterm.recv().await;
        signal::kill(tests_pid, Signal::SIGTERM).unwrap();
    });

    // Tests either terminate normally, or with SIGTERM.
    if !tests_handle.await.unwrap().success() {
        panic!("Tests failed.");
    }
}
