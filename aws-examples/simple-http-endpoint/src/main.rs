extern crate lambda_runtime as lambda;
extern crate log;
extern crate serde_derive;
extern crate chrono;

use lambda::{error::HandlerError, lambda};
use serde_derive::{Deserialize, Serialize};
use std::error::Error;
use lambda::Context;
use chrono::{Timelike, Utc};

#[derive(Deserialize)]
struct CustomEvent {
    
} // empty struct, unit struct does not work

#[derive(Serialize)]
struct CustomOutput {
    message: String,
}

fn main() -> Result<(), Box<dyn Error>> {
    lambda!(my_handler);

    Ok(())
}

fn my_handler(_: CustomEvent, _: Context) -> Result<CustomOutput, HandlerError> {
    let now = Utc::now();
    let (is_pm, hour) = now.hour12();

    Ok(CustomOutput {
        message: format!("Hello, the current time is {:02}:{:02}:{:02} {}",
            hour,
            now.minute(),
            now.second(),
            if is_pm { "PM" } else { "AM" }
        )
    })
}