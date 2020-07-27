use crate::error::CLIResult;

//use std::env;
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

    /*
    pub fn deploy(&self) -> CLIResult<String> {
        let mut path: Vec<&str> = env!("CARGO_MANIFEST_DIR").split("/").collect();
        path.truncate(path.len()-2);
        path.push("deploy.sh");
        let command = path.join("/");
        println!("{:?}", command);
        unimplemented!();
        //let output = Command::new(command).output()?;
        //Ok(output)
    }
    */
    
    pub async fn create_function(&self, name: &str, filename: &str) -> CLIResult<String> {
        Ok(reqwest::Client::new()
            .post(&format!("{}/create-function/{}", self.storage, name))
            .json(&json!({
                "contents": fs::read_to_string(filename)?
            }))
            .send()
            .await?
            .text()
            .await?)
    }

    pub async fn delete_function(&self, name: &str) -> CLIResult<String> {
        Ok(reqwest::get(&format!("{}/delete-function/{}", self.storage, name)).await?.text().await?)
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
}