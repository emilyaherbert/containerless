#[macro_use]
extern crate log;
use duct::cmd;
use reqwest;
use std::fs::File;
use std::io::copy;
use std::time::Duration;
use thiserror::Error;
use tokio::time::delay_for;

#[derive(Debug, Error)]
enum DownloadError {
    #[error("network error downloading dispatcher-agent: {0}")]
    Reqwest(#[from] reqwest::Error),
    #[error("HTTP status code {0} downloading dispatcher agent")]
    StatusCode(reqwest::StatusCode),
}

async fn download_dispatcher_err() -> Result<bytes::Bytes, DownloadError> {
    let resp = reqwest::get("http://controller/download_dispatcher").await?;
    if !resp.status().is_success() {
        return Err(DownloadError::StatusCode(resp.status()));
    }
    return Ok(resp.bytes().await?);
}

async fn download_dispatcher() -> bytes::Bytes {
    loop {
        match download_dispatcher_err().await {
            Err(err) => {
                error!(target: "dispatcher-launcher", "{}", err);
                delay_for(Duration::from_secs(1)).await;
            }
            Ok(bytes) => {
                return bytes;
            }
        }
    }
}

#[tokio::main]
async fn main() {
    env_logger::init();

    let response = download_dispatcher().await;
    info!(target: "dispatcher-launcher", "INITIALIZE: downloaded the Dispatcher from the Controller");
    let mut resp_slice = response.as_ref();
    {
        let mut dest = File::create("/dispatcher").expect("creating /dispatcher");
        copy(&mut resp_slice, &mut dest).expect("Saving Dispatcher");
    }
    cmd!("chmod", "a+x", "/dispatcher")
        .run()
        .expect("setting executable permissions");
    info!(target: "dispatcher-launcher", "INITIALIZE: launching the Dispatcher");
    // Unix exec: replace the current process, preserving PID. This is needed
    // so that the dispatcher receives SIGTERM.
    let err = exec::Command::new("/dispatcher").exec();
    eprintln!("exec failed: {}", err);
}
