use duct::cmd;
use log::{error, info};
use reqwest;
use std::fs::File;
use std::io::copy;
use std::time::Duration;
use tokio::time::delay_for;

async fn download_dispatcher() -> bytes::Bytes {
    loop {
        match reqwest::get("http://controller/download_dispatcher").await {
            Err(err) => {
                error!(target: "dispatcher-launcher", "GET download_dispatcher failed {}", err);
                delay_for(Duration::from_secs(1)).await;
            }
            Ok(response) => {
                if !response.status().is_success() {
                    error!(target: "dispatcher-launcher", "GET download_dispatcher returned code {}", response.status());
                    delay_for(Duration::from_secs(1)).await;
                    continue;
                }
                match response.bytes().await {
                    Err(err) => {
                        error!(target: "dispatcher-launcher", "Error downloading dispatcher {}", err);
                        delay_for(Duration::from_secs(1)).await;
                    }
                    Ok(bytes) => {
                        return bytes;
                    }
                }
            }
        }
    }
}

#[tokio::main]
async fn main() {
    env_logger::init();

    let response = download_dispatcher().await;
    info!(target: "dispatcher-launcher", "downloaded the Dispatcher from the Controller");
    let mut resp_slice = response.as_ref();
    {
        let mut dest = File::create("/dispatcher").expect("creating /dispatcher");
        copy(&mut resp_slice, &mut dest).expect("Saving Dispatcher");
    }
    cmd!("chmod", "a+x", "/dispatcher")
        .run()
        .expect("setting executable permissions");
    info!(target: "dispatcher-launcher", "Launching the Dispatcher");
    // Unix exec: replace the current process, preserving PID
    let err = exec::Command::new("/dispatcher").exec();
    eprintln!("exec failed: {}", err);
}
