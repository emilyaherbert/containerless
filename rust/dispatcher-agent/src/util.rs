use super::error::*;
use crate::types::*;
use futures::future::FutureExt;
use futures::prelude::*;
use futures_retry::{FutureRetry, RetryPolicy};
use http::uri::Uri;
use std::time::Duration;

pub async fn maybe_run<E, F>(flag: bool, f: F) -> Result<(), E>
where
    F: Future<Output = Result<(), E>>,
{
    if flag {
        return f.await;
    } else {
        return Ok(());
    }
}

pub async fn retry_get(
    client: &HttpClient,
    mut tries: usize,
    delay_secs: u64,
    uri: Uri,
) -> Result<(), ()> {
    let mk_req = || {
        hyper::Request::builder()
            .uri(uri.clone())
            .body(hyper::Body::from(""))
            .unwrap()
    };
    let resp_result = FutureRetry::new(
        move || {
            client.request(mk_req()).map(|resp| match resp {
                Ok(resp) => {
                    if resp.status().is_success() {
                        Ok(())
                    } else {
                        Err(())
                    }
                }
                Err(err) => {
                    eprintln!("Request error: {}", err);
                    Err(())
                }
            })
        },
        move |err| {
            eprintln!("Pinging ({} tries left)", tries);
            if tries == 0 {
                return RetryPolicy::ForwardError(err);
            }
            tries = tries - 1;
            return RetryPolicy::WaitRetry(Duration::from_secs(delay_secs));
        },
    ).await;
    return match resp_result {
        Ok(_) => Ok(()),
        Err(_) => Err(())
    };
}

pub async fn wait_for_service(
    http_client: &HttpClient,
    authority: uri::Authority,
) -> Result<(), Error> {
    let uri = http::Uri::builder()
        .scheme("http")
        .authority(authority)
        .path_and_query("/readinessProbe")
        .build()
        .unwrap();
    if let Err(_err) = retry_get(http_client, 30, 1, uri).await {
        return Err(Error::Timeout);
    }
    return Ok(());
}

pub async fn log_error<F, E, S>(future: F, message: S)
where
    F: Future<Output = Result<(), E>>,
    E: std::error::Error,
    S: Into<String>,
{
    let message = message.into();
    if let Err(err) = future.await {
        debug!(target: "dispatcher", "{}, error is {:?}", message, err);
    }
}
