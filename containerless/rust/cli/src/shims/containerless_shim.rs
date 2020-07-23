use crate::error::CLIResult;

use std::process::Command;
use std::env;

pub fn deploy() -> CLIResult<String> {
    let mut path: Vec<&str> = env!("CARGO_MANIFEST_DIR").split("/").collect();
    path.truncate(path.len()-2);
    path.push("deploy.sh");
    let command = path.join("/");
    println!("{:?}", command);
    unimplemented!();
    //let output = Command::new(command).output()?;
    //Ok(output)
}