use crate::error::Error;

use std::process::Command;

type CLIResult<T> = Result<T, Error>;

pub fn get_all() -> CLIResult<String> {
    let output = Command::new("microk8s.kubectl")
        .args(&["get", "all", "-n", "containerless"])
        .output()?;
    let stdout_str = String::from_utf8(output.stdout)?;
    Ok(stdout_str)
}