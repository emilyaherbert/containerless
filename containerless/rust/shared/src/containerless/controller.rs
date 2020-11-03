use crate::containerless::error::Error;

use serde_json::json;
use std::fs;

pub async fn create_function(
    name: &str, filename: &str, containers_only: bool,
) -> Result<String, Error> {
    Ok(reqwest::Client::new()
        .post(&format!(
            "http://localhost/controller/create_function/{}?containers_only={}",
            name, containers_only
        ))
        .json(&json!({
            "contents": format!("{}", fs::read_to_string(filename)?.trim())
        }))
        .send()
        .await?
        .text()
        .await?)
}

pub async fn delete_function(name: &str) -> Result<String, Error> {
    Ok(reqwest::get(&format!(
        "http://localhost/controller/delete_function/{}",
        name
    ))
    .await?
    .text()
    .await?)
}

pub async fn shutdown_function_instances(name: &str) -> Result<String, Error> {
    Ok(reqwest::get(&format!(
        "http://localhost/controller/shutdown_function_instances/{}",
        name
    ))
    .await?
    .text()
    .await?)
}

pub async fn reset_function(name: &str) -> Result<String, Error> {
    Ok(reqwest::get(&format!(
        "http://localhost/controller/reset_function/{}",
        name
    ))
    .await?
    .text()
    .await?)
}

pub async fn get_function(name: &str) -> Result<String, Error> {
    Ok(reqwest::get(&format!(
        "http://localhost/controller/get_function/{}",
        name
    ))
    .await?
    .text()
    .await?)
}

pub async fn list_functions() -> Result<String, Error> {
    Ok(reqwest::get("http://localhost/controller/list_functions")
        .await?
        .text()
        .await?)
}

pub async fn dispatcher_version() -> Result<String, Error> {
    Ok(
        reqwest::get("http://localhost/controller/dispatcher_version")
            .await?
            .text()
            .await?,
    )
}
