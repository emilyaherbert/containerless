#[macro_use]
extern crate lambda_runtime as lambda;
#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate log;
extern crate simple_logger;
extern crate aws_sdk_rust;

use lambda::error::HandlerError;

use std::error::Error;

use aws_sdk_rust::aws::common::credentials::DefaultCredentialsProvider;
use aws_sdk_rust::aws::s3::bucket::*;
use aws_sdk_rust::aws::s3::object::*;
use aws_sdk_rust::aws::s3::acl::*;

use aws_sdk_rust::aws::common::region::Region;
use aws_sdk_rust::aws::s3::endpoint::{Endpoint, Signature};
use aws_sdk_rust::aws::s3::s3client::S3Client;

#[derive(Deserialize, Clone)]
struct CustomEvent {
    #[serde(rename = "firstName")]
    first_name: String,
}

#[derive(Serialize, Clone)]
struct CustomOutput {
    message: String,
}

fn main() -> Result<(), Box<dyn Error>> {
    simple_logger::init_with_level(log::Level::Info)?;
    lambda!(my_handler);

    Ok(())
}

fn my_handler(e: CustomEvent, c: lambda::Context) -> Result<CustomOutput, HandlerError> {
    let provider = DefaultCredentialsProvider::new(None).unwrap();

    let endpoint = Endpoint::new(Region::UsEast1, Signature::V4, None, None, None, None);
    let client = S3Client::new(provider, endpoint);

    let bucket_name: &str = "test-bucket-318629";

    let mut put_object = PutObjectRequest::default();
    put_object.bucket = bucket_name.to_string();
    put_object.key = "mytest.txt".to_string();
    put_object.body = Some(b"this is a test.");

    let result: String = match client.put_object(&put_object, None) {
        Ok(output) =>  String::from(format!("{:#?}", output)),
        Err(e) => String::from(format!("{:#?}", e)),
    };

    Ok(CustomOutput {
        message: format!("Result: {}!", result),
    })
}
