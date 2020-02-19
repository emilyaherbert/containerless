use crate::types::*;
use futures::future::FutureExt;
use futures_retry::{FutureRetry, RetryPolicy};
use http::uri::Uri;
use std::time::Duration;

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
    let _resp = FutureRetry::new(
        move || {
            client.request(mk_req()).map(|resp| match resp {
                Ok(resp) => {
                    if resp.status().is_success() {
                        Ok(())
                    } else {
                        Err(())
                    }
                }
                Err(_err) => Err(()),
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
    )
    .await?;
    return Ok(());
}
