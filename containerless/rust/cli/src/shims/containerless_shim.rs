use crate::error::*;

use std::fs;
use serde_json::json;

pub struct ContainerlessShim {
    dispatcher: String,
    controller: String
}

impl ContainerlessShim {
    pub fn new() -> ContainerlessShim {
        ContainerlessShim {
            dispatcher: "http://localhost/dispatcher".to_string(),
            controller: "http://localhost/controller".to_string()
        }
    }
    
    pub async fn create_function(&self, name: &str, filename: &str) -> CLIResult<String> {
        Ok(reqwest::Client::new()
            .post(&format!("{}/create_function/{}", self.controller, name))
            .json(&json!({
                "contents": format!("{}", fs::read_to_string(filename)?.trim())
            }))
            .send()
            .await?
            .text()
            .await?)
    }

    pub async fn delete_function(&self, name: &str) -> CLIResult<String> {
        Ok(reqwest::get(&format!("{}/delete_function/{}", self.controller, name)).await?.text().await?)
    }

    pub async fn shutdown_function_instances(&self, name: &str) -> CLIResult<String> {
        Ok(reqwest::get(&format!("{}/shutdown_function_instances/{}", self.controller, name)).await?.text().await?)
    }

    pub async fn get_function(&self, name: &str) -> CLIResult<String> {
        Ok(reqwest::get(&format!("{}/get_function/{}", self.controller, name)).await?.text().await?)
    }

    pub async fn list_functions(&self) -> CLIResult<String> {
        Ok(reqwest::get(&format!("{}/list_functions", self.controller)).await?.text().await?)
    }

    pub async fn invoke(&self, name: &str) -> CLIResult<String> {
        Ok(reqwest::get(&format!("{}/{}/foo", self.dispatcher, name)).await?.text().await?)
    }
}