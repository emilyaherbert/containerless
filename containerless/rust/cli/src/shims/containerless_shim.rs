use crate::error::CLIResult;

use std::process::Command;
use std::env;

pub struct ContainerlessShim {
    storage: String
}

impl ContainerlessShim {
    pub fn new() -> ContainerlessShim {
        ContainerlessShim {
            storage: "http://localhost/storage".to_string()
        }
    }

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
    
    pub fn create_function(&self, name: &str) -> CLIResult<String> {
        let output = Command::new("curl")
            .arg(format!("{}/create-function/{}", self.storage, name))
            .output()?;
        let stdout_str = String::from_utf8(output.stdout)?;
        Ok(stdout_str)
    }

    pub fn delete_function(&self, name: &str) -> CLIResult<String> {
        let output = Command::new("curl")
            .arg(format!("{}/delete-function/{}", self.storage, name))
            .output()?;
        let stdout_str = String::from_utf8(output.stdout)?;
        Ok(stdout_str)
    }

    pub fn describe_function(&self, name: &str) -> CLIResult<String> {
        let output = Command::new("curl")
            .arg(format!("{}/get-function/{}", self.storage, name))
            .output()?;
        let stdout_str = String::from_utf8(output.stdout)?;
        Ok(stdout_str)
    }

    pub fn list_functions(&self) -> CLIResult<String> {
        let output = Command::new("curl")
            .arg(format!("{}/list-functions", self.storage))
            .output()?;
        let stdout_str = String::from_utf8(output.stdout)?;
        Ok(stdout_str)
    }
}