
use shared::response;
use crate::error::Error;

use std::fs::File;
use std::io::prelude::*;
use std::collections::HashMap;

pub async fn upload_handler(contents: HashMap<String, String>) -> Result<impl warp::Reply, warp::Rejection> {
    info!(target: "local-database", "entered upload handler");
    //return response::ok_response(r#"{ "body": "got a response back from the server\n" }"#.to_string());

    if !contents.contains_key("body") {
        return response::error_response(r#"{ "error": "file not properly formatted\n" }"#.to_string());
    }

    if let Err(_err) = upload(contents.get("body").unwrap().to_string()).await {
        return response::error_response(r#"{ "error": "error uploading file\n" }"#.to_string());
    }

    return response::ok_response(r#"{ "body": "Done uploading!\n" }"#.to_string());
}

pub async fn upload(contents: String) -> Result<(), Error> {
    let mut file = File::create("file.txt")?;
    Ok(file.write_all(contents.as_bytes())?)
}