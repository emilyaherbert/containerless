use crate::error::*;

use std::fs;
use serde_json::json;

pub struct ContainerlessShim {
    storage: String,
    dispatcher: String
}

impl ContainerlessShim {
    pub fn new() -> ContainerlessShim {
        ContainerlessShim {
            storage: "http://localhost/storage".to_string(),
            dispatcher: "http://localhost/dispatcher".to_string()
        }
    }
    
    pub async fn create_function(&self, name: &str, filename: &str) -> CLIResult<()> {
        let acceptable_chars: Vec<char> = "abcdefghijklmnopqrstuvwxyz1234567890.-".chars().collect();
        if !name.chars().all(|c| acceptable_chars.contains(&c)) {
            return Err(Error::Parsing("Function names can only contain lower case alphanumeric characters, '.', and ','.".to_string()));
        }
        println!("Adding function to storage...");
        println!("{}", self.add_to_storage(name, filename).await?);
        Ok(())
    }

    pub async fn delete_function(&self, name: &str) -> CLIResult<()> {
        println!("Deleting from storage...");
        println!("{}", self.delete_from_storage(name).await?);
        Ok(())
    }

    pub async fn describe_function(&self, name: &str) -> CLIResult<String> {
        Ok(reqwest::get(&format!("{}/get-function/{}", self.storage, name)).await?.text().await?)
    }

    pub async fn list_functions(&self) -> CLIResult<String> {
        Ok(reqwest::get(&format!("{}/list-functions", self.storage)).await?.text().await?)
    }

    pub async fn invoke(&self, name: &str) -> CLIResult<String> {
        Ok(reqwest::get(&format!("{}/{}/foo", self.dispatcher, name)).await?.text().await?)
    }

    async fn add_to_storage(&self, name: &str, filename: &str) -> CLIResult<String> {
        Ok(reqwest::Client::new()
            .post(&format!("{}/create-function/{}", self.storage, name))
            .json(&json!({
                "contents": format!("{}", fs::read_to_string(filename)?.trim())
            }))
            .send()
            .await?
            .text()
            .await?)
    }

    async fn delete_from_storage(&self, name: &str) -> CLIResult<String> {
        Ok(reqwest::get(&format!("{}/delete-function/{}", self.storage, name)).await?.text().await?)
    }
}