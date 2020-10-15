use crate::common::*;
use crate::response;

use tokio::process::Command;
use std::process::Stdio;
use std::io;
use serde_json::Value as JsonValue;

pub async fn containerless_create<E>(name: &str, js_code: &str, containers_only: bool) -> Result<std::process::Output, E> 
    where E: std::convert::From<io::Error> {
    let filename = format!("{}/_code.js", ROOT.as_str());
    std::fs::write(&filename, js_code)?;
    let mut args = vec!("create", "-n", name, "-f", &filename);
    if containers_only {
        args.push("--containers_only");
    }
    Ok(Command::new(format!("{}/debug/cli", ROOT.as_str()))
        .args(args)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .await?)
}

pub async fn containerless_delete<E>(name: &str) -> Result<std::process::Output, E> 
    where E: std::convert::From<io::Error> {
    Ok(Command::new(format!("{}/debug/cli", ROOT.as_str()))
        .args(vec!("delete", "-n", name))
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .await?)
}

pub async fn containerless_remove_containers<E>(name: &str) -> Result<std::process::Output, E> 
    where E: std::convert::From<io::Error> {
    Ok(Command::new(format!("{}/debug/cli", ROOT.as_str()))
        .args(vec!("remove-containers", "-n", name))
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .await?)
}

pub async fn containerless_remove_trace<E>(name: &str) -> Result<std::process::Output, E> 
    where E: std::convert::From<io::Error> {
    Ok(Command::new(format!("{}/debug/cli", ROOT.as_str()))
        .args(vec!("remove-trace", "-n", name))
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .await?)
}

pub async fn containerless_invoke<E, F>(name: &str, req: (&str, JsonValue), err_case: F) -> Result<String, E> 
    where E: std::convert::From<io::Error> + std::convert::From<reqwest::Error>,
          F: FnOnce(std::string::String) -> E {
    let (path, body) = req;
    let resp = reqwest::Client::new()
        .post(&format!("http://localhost/dispatcher/{}/{}", name, path))
        .json(&body)
        .send()
        .await?;
    response::response_into_result(resp.status().as_u16(), resp.text().await?).map_err(err_case)
}

pub async fn containerless_compile<E>(name: &str) -> Result<std::process::Output, E> 
    where E: std::convert::From<io::Error> {
    Ok(Command::new(format!("{}/debug/cli", ROOT.as_str()))
        .args(vec!("compile", "-n", name))
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .await?)
}

pub async fn containerless_get_dispatcher_version<E>() -> Result<std::process::Output, E> 
    where E: std::convert::From<io::Error> {
    Ok(Command::new(format!("{}/debug/cli", ROOT.as_str()))
        .args(vec!("dispatcher-version"))
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .await?)
}