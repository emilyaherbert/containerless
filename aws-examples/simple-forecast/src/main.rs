#[macro_use]
extern crate lambda_runtime as lambda;
#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate log;
extern crate simple_logger;

extern crate reqwest;
extern crate forecast;

use lambda::error::HandlerError;

use std::error::Error;

use reqwest::Client;

use forecast::{ApiClient, ForecastRequestBuilder,
               ExcludeBlock, ExtendBy, Lang, Units};

#[derive(Deserialize, Clone)]
struct CustomEvent {
    #[serde(rename = "firstName")]
    api_key: String,
    latitude: String,
    longitude: String
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

fn get_forecast(api_key: &str, latitude: f64, longitude: f64) -> String {
    let reqwest_client = Client::new();
    let api_client = ApiClient::new(&reqwest_client);

    let mut blocks = vec![ExcludeBlock::Daily, ExcludeBlock::Alerts];

    let forecast_request = ForecastRequestBuilder::new(api_key, latitude, longitude)
        .exclude_block(ExcludeBlock::Hourly)
        .exclude_blocks(&mut blocks)
        .extend(ExtendBy::Hourly)
        .lang(Lang::Arabic)
        .units(Units::Imperial)
        .build();

    let forecast_response = api_client.get_forecast(forecast_request).unwrap();

    format!("{:?}", forecast_response)
}

fn my_handler(e: CustomEvent, c: lambda::Context) -> Result<CustomOutput, HandlerError> {
    let lat = String::from(e.latitude);
    let lat = lat.parse::<f64>().unwrap();
    let long = String::from(e.longitude);
    let long = long.parse::<f64>().unwrap();
    let api_key = String::from(e.api_key);

    Ok(CustomOutput {
        message: get_forecast(&api_key, lat, long)
    })
}
