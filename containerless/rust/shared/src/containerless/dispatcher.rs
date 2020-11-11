use crate::containerless::error::Error;
use crate::response::*;
use duct::cmd;

pub fn dispatcher_ip() -> String {
    return cmd!("microk8s.kubectl", "-n", "containerless", "get",
      "svc/exposed-dispatcher", "-o", "json")
      .pipe(cmd!("jq", "-j", ".spec.clusterIP"))
      .read()
      .expect("error reading dispatcher IP");
}

pub async fn invoke(name: &str) -> Result<String, Error> {
    Ok(
        reqwest::get(&format!("http://{}:8080/{}/foo", dispatcher_ip(), name))
            .await?
            .text()
            .await?,
    )
}

pub async fn compile(name: &str) -> Result<String, Error> {
    Ok(reqwest::Client::new()
        .post(&format!("http://{}:8080/compile/{}", dispatcher_ip(), name))
        .body("".to_string())
        .send()
        .await?
        .text()
        .await?)
}

pub async fn shutdown_function_instances(name: &str) -> Result<String, Error> {
    let resp = reqwest::get(&format!(
        "http://{}:8080/shutdown_function_instances/{}",
        dispatcher_ip(),
        name
    ))
    .await?;
    response_into_result(resp.status().as_u16(), resp.text().await?).map_err(Error::Dispatcher)
}
