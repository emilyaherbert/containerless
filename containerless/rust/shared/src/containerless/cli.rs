use crate::common::*;
use crate::containerless::error::Error;
use crate::response;

use serde_json::Value as JsonValue;
use std::process::Stdio;
use tokio::process::Command;

pub async fn containerless_create(
    name: &str, js_code: &str, containers_only: bool,
) -> Result<std::process::Output, Error> {
    let filename = format!("{}/_code.js", ROOT.as_str());
    std::fs::write(&filename, js_code)?;
    let mut args = vec!["create", "-n", name, "-f", &filename];
    if containers_only {
        args.push("--containers-only");
    }
    Ok(Command::new(format!("{}/{}/cli", ROOT.as_str(), BUILD_VERSION.as_str()))
        .args(args)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .await?)
}

pub async fn containerless_delete(name: &str) -> Result<std::process::Output, Error> {
    Ok(Command::new(format!("{}/{}/cli", ROOT.as_str(), BUILD_VERSION.as_str()))
        .args(vec!["delete", "-n", name])
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .await?)
}

pub async fn containerless_remove_containers(name: &str) -> Result<std::process::Output, Error> {
    Ok(Command::new(format!("{}/{}/cli", ROOT.as_str(), BUILD_VERSION.as_str()))
        .args(vec!["remove-containers", "-n", name])
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .await?)
}

pub async fn containerless_remove_trace(name: &str) -> Result<std::process::Output, Error> {
    Ok(Command::new(format!("{}/{}/cli", ROOT.as_str(), BUILD_VERSION.as_str()))
        .args(vec!["remove-trace", "-n", name])
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .await?)
}

pub async fn containerless_invoke(name: &str, req: (&str, JsonValue)) -> Result<String, Error> {
    let (path, body) = req;
    let url = format!("http://{}:8080/{}{}", super::dispatcher::dispatcher_ip(), name, path);
    let resp = reqwest::Client::new()
        .post(&url)
        .json(&body)
        .send()
        .await?;
    response::response_into_result(resp.status().as_u16(), resp.text().await?)
        .map_err(Error::Invoke)
}

pub async fn containerless_compile(name: &str) -> Result<std::process::Output, Error> {
    Ok(Command::new(format!("{}/{}/cli", ROOT.as_str(), BUILD_VERSION.as_str()))
        .args(vec!["compile", "-n", name])
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .await?)
}

pub async fn containerless_get_dispatcher_version() -> Result<std::process::Output, Error> {
    Ok(Command::new(format!("{}/{}/cli", ROOT.as_str(), BUILD_VERSION.as_str()))
        .args(vec!["dispatcher-version"])
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .await?)
}
