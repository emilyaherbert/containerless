use reqwest::IntoUrl;
use std::fmt::Display;
use std::time::{Duration, Instant};
use thiserror::Error;
use tokio::time;

#[derive(Error, Debug)]
#[error("timeout error: {reason}")]
pub struct TimeoutError {
    pub reason: String,
}

async fn poll_url_internal<'a, U>(
    client: &'a reqwest::Client, url: U, interval: Duration, timeout: Option<Duration>,
) -> Result<(), TimeoutError>
where
    U: IntoUrl + Clone + Display,
{
    let end_time = timeout.map(|t| Instant::now() + t);

    loop {
        if let Ok(resp) = client.get(url.clone()).send().await {
            if resp.status().is_success() {
                return Ok(());
            }
        }
        if let Some(t) = end_time {
            if Instant::now() >= t {
                return Err(TimeoutError {
                    reason: format!("polling {}", &url),
                });
            }
        }
        time::delay_for(interval).await;
    }
}

pub async fn poll_url_no_timeout<'a, U>(
    client: &'a reqwest::Client, url: U, interval: Duration,
) -> ()
where
    U: IntoUrl + Clone + Display,
{
    poll_url_internal(client, url, interval, None)
        .await
        .unwrap();
}

pub async fn poll_url_with_timeout<'a, U>(
    client: &'a reqwest::Client, url: U, interval: Duration, timeout: Duration,
) -> Result<(), TimeoutError>
where
    U: IntoUrl + Clone + Display,
{
    return poll_url_internal(client, url, interval, Some(timeout)).await;
}