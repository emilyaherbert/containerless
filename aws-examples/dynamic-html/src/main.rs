extern crate aws_lambda as lambda;
extern crate url;

use url::Url;
use std::collections::HashMap;

fn main() {
    lambda::gateway::start(|req| {
        let lambda_uri = req.uri().to_string();
        let parsed_url = Url::parse(&lambda_uri).unwrap();
        let query_map: HashMap<_, _> = parsed_url.query_pairs().into_owned().collect();

        let res = lambda::gateway::response()
            .status(200)
            .body(format!("Hello ƛ! {}", query_map.get("name").unwrap()).into())?;
        Ok(res)
    })
}