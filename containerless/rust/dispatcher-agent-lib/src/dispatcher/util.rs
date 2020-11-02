use super::types::*;
use crate::error::*;
use futures::future::FutureExt;
use futures::prelude::*;
use futures_retry::{FutureRetry, RetryPolicy};
use http::uri::Uri;
use std::time::Duration;
use tokio::time::delay_for;

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
    client: &HttpClient, mut tries: usize, delay_secs: u64, uri: Uri,
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
                Err(_err) => Err(()),
            })
        },
        move |err| {
            if tries == 0 {
                return RetryPolicy::ForwardError(err);
            }
            tries -= 1;
            return RetryPolicy::WaitRetry(Duration::from_secs(delay_secs));
        },
    )
    .await;
    return match resp_result {
        Ok(_) => Ok(()),
        Err(_) => Err(()),
    };
}

pub async fn wait_for_service(
    http_client: &HttpClient, authority: uri::Authority,
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

pub async fn wait_for_pod_running(
    k8s_client: &K8sClient, pod_name: &str, timeout_secs: usize,
) -> Result<(), Error> {
    use k8s::{PodCondition, PodPhase};
    for _i in 0..timeout_secs {
        let (phase, ready) = k8s_client.get_pod_phase_and_readiness(pod_name).await?;
        match (phase, ready) {
            (PodPhase::Failed, _) => {
                return Err(Error::UnexpectedPodPhase(pod_name.to_string(), phase))
            }
            (PodPhase::Succeeded, _) => {
                return Err(Error::UnexpectedPodPhase(pod_name.to_string(), phase))
            }
            (PodPhase::Pending, _) => (),
            (PodPhase::Unknown, _) => (),
            (PodPhase::Running, PodCondition::True) => return Ok(()),
            (PodPhase::Running, _) => (),
        }
        delay_for(Duration::from_secs(1)).await;
    }
    return Err(Error::TimeoutReason(format!(
        "timeout waiting for pod {} to enter Running phase",
        pod_name
    )));
}

pub fn text_response(code: u16, text: impl Into<String>) -> hyper::Response<hyper::Body> {
    return hyper::Response::builder()
        .status(code)
        .body(hyper::Body::from(text.into()))
        .unwrap();
}

pub fn send_log_error<T>(sender: futures::channel::oneshot::Sender<T>, value: T)
where
    T: std::fmt::Debug,
{
    if sender.is_canceled() {
        error!(target: "dispatcher", "trying to send a message to a dropped reciever");
    } else {
        if let Err(value) = sender.send(value) {
            error!(target: "dispatcher", "could not send {:?}", value);
        }
    }
}
