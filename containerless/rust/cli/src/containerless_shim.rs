use crate::error::*;

use serde_json::json;
use std::fs;

pub struct ContainerlessShim {
    dispatcher: String,
    controller: String,
}

impl ContainerlessShim {
    pub fn new() -> ContainerlessShim {
        ContainerlessShim {
            dispatcher: "http://localhost/dispatcher".to_string(),
            controller: "http://localhost/controller".to_string(),
        }
    }

    pub async fn create_function(&self, name: &str, filename: &str, containers_only: bool) -> CLIResult<String> {
        Ok(reqwest::Client::new()
            .post(&format!("{}/create_function/{}?containers_only={}", self.controller, name, containers_only))
            .json(&json!({
                "contents": format!("{}", fs::read_to_string(filename)?.trim())
            }))
            .send()
            .await?
            .text()
            .await?)
    }

    pub async fn delete_function(&self, name: &str) -> CLIResult<String> {
        Ok(
            reqwest::get(&format!("{}/delete_function/{}", self.controller, name))
                .await?
                .text()
                .await?,
        )
    }

    pub async fn shutdown_function_instances(&self, name: &str) -> CLIResult<String> {
        Ok(reqwest::get(&format!(
            "{}/shutdown_function_instances/{}",
            self.controller, name
        ))
        .await?
        .text()
        .await?)
    }

    pub async fn reset_function(&self, name: &str) -> CLIResult<String> {
        Ok(
            reqwest::get(&format!("{}/reset_function/{}", self.controller, name))
                .await?
                .text()
                .await?,
        )
    }

    pub async fn get_function(&self, name: &str) -> CLIResult<String> {
        Ok(
            reqwest::get(&format!("{}/get_function/{}", self.controller, name))
                .await?
                .text()
                .await?,
        )
    }

    pub async fn list_functions(&self) -> CLIResult<String> {
        Ok(reqwest::get(&format!("{}/list_functions", self.controller))
            .await?
            .text()
            .await?)
    }

    pub async fn invoke(&self, name: &str) -> CLIResult<String> {
        Ok(reqwest::get(&format!("{}/{}/foo", self.dispatcher, name))
            .await?
            .text()
            .await?)
    }

    pub async fn dispatcher_version(&self) -> CLIResult<String> {
        Ok(reqwest::get(&format!("{}/dispatcher_version", self.controller))
            .await?
            .text()
            .await?)
    }

    pub async fn compile(&self, name: &str) -> CLIResult<String> {
        Ok(reqwest::Client::new()
            .post(&format!("{}/compile/{}", self.dispatcher, name))
            .body("".to_string())
            .send()
            .await?
            .text()
            .await?)
    }
}
