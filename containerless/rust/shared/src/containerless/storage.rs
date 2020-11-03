use crate::containerless::error::Error;
use crate::function::*;
use crate::response::*;

pub async fn list() -> Result<String, Error> {
    let resp = reqwest::get("http://localhost/storage/list_functions").await?;
    response_into_result(resp.status().as_u16(), resp.text().await?).map_err(Error::Storage)
}

pub async fn get(name: &str) -> Result<String, Error> {
    let resp = reqwest::get(&format!("http://localhost/storage/get_function/{}", name)).await?;
    response_into_result(resp.status().as_u16(), resp.text().await?).map_err(Error::Storage)
}

pub async fn get_internal(name: &str) -> Result<String, Error> {
    let resp = reqwest::get(&format!("http://storage:8080/get_function/{}", name)).await?;
    response_into_result(resp.status().as_u16(), resp.text().await?).map_err(Error::Storage)
}

pub async fn add(name: &str, contents: FunctionContents) -> Result<String, Error> {
    let resp = reqwest::Client::new()
        .post(&format!(
            "http://localhost/storage/create_function/{}",
            name
        ))
        .json(&contents)
        .send()
        .await?;
    response_into_result(resp.status().as_u16(), resp.text().await?).map_err(Error::Storage)
}

pub async fn delete(name: &str) -> Result<String, Error> {
    let resp = reqwest::get(&format!(
        "http://localhost/storage/delete_function/{}",
        name
    ))
    .await?;
    response_into_result(resp.status().as_u16(), resp.text().await?).map_err(Error::Storage)
}
