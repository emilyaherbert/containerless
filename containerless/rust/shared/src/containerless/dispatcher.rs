use crate::containerless::error::Error;
use crate::response::*;

pub async fn invoke(name: &str) -> Result<String, Error> {
    Ok(reqwest::get(&format!("http://localhost/dispatcher/{}/foo", name))
        .await?
        .text()
        .await?)
}

pub async fn compile(name: &str) -> Result<String, Error> {
    Ok(reqwest::Client::new()
        .post(&format!("http://localhost/dispatcher/compile/{}", name))
        .body("".to_string())
        .send()
        .await?
        .text()
        .await?)
}

pub async fn shutdown_function_instances(name: &str) -> Result<String, Error> {
    let resp = reqwest::get(&format!(
        "http://localhost/dispatcher/shutdown_function_instances/{}",
        name
    ))
    .await?;
    response_into_result(resp.status().as_u16(), resp.text().await?).map_err(Error::Dispatcher)
}