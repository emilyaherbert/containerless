extern crate aws_lambda as lambda;
extern crate chrono;

use chrono::{Timelike, Utc};

fn main() {
    lambda::gateway::start(|_req| {
        let now = Utc::now();
        let (is_pm, hour) = now.hour12();
        let res = lambda::gateway::response()
            .status(200)
            .body(format!("Hello, the current time is {:02}:{:02}:{:02} {}",
            hour,
            now.minute(),
            now.second(),
            if is_pm { "PM" } else { "AM" }).into())?;
        Ok(res)
    })
}