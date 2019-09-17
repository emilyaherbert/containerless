use super::error::Error;
use super::type_dynamic::{Dyn, DynResult};

use std::io::{self, Write};

use hyper::{Client, Uri};
use hyper::rt::{self, Future, Stream};
use hyper_tls::HttpsConnector;
use tokio::runtime::Runtime;

pub struct ExecutionContext {
    pub events: Vec<(String, i32)>
}

// https://zsiciarz.github.io/24daysofrust/book/vol1/day5.html
// https://stackoverflow.com/questions/14154753/how-do-i-make-an-http-request-from-rust/14189088#14189088
// https://stackoverflow.com/questions/48573070/no-method-named-post-found-for-type-hyperclient-in-hyper-0-11
// https://docs.rs/hyper/0.12.35/hyper/client/index.html
// https://github.com/hyperium/hyper/blob/0.12.x/examples/client.rs
// https://github.com/tokio-rs/tokio/issues/1412#issuecomment-519667025
// https://hyper.rs/guides/client/basic/

impl ExecutionContext {
    pub fn loopback<'a>(
        &mut self,
        event_name: &'static str,
        event_arg: DynResult<'a>,
        _event_clos: DynResult<'a>,
        loopback_id: i32,
    ) -> DynResult<'a>
    {
        if event_name == "listen" {

            self.events.push((event_name.to_string(), loopback_id));
            Dyn::int(0)

        } else if event_name == "get" {
            let mut rt = Runtime::new().unwrap();
            rt.block_on(
                fetch_url_https(Uri::from_static("https://emilyaherbert.github.io/authorize.txt"))
                    .and_then(|res| {
                        println!("{:?}", res);
                        Ok(())
                    })
            ).unwrap();
            rt.shutdown_now().wait().expect("Could not shutdown Tokio");

            self.events.push((event_name.to_string(), loopback_id));
            Dyn::int(0)
        }
        else {
            Dyn::int(0)
        }
    }

    pub fn send<'a>(&mut self, _value: Dyn<'a>) -> DynResult<'a> {
        Dyn::int(0)
    }

    pub fn new() -> ExecutionContext {
        ExecutionContext {
            events: Vec::new()
        }
    }
}

// https://github.com/hyperium/hyper/blob/0.12.x/examples/client.rs
fn fetch_url_http(url: hyper::Uri) -> impl Future<Item=(), Error=()> {
    let client = Client::new();

    client
        .get(url)
        .and_then(|res| {
            //println!("Response: {}", res.status());
            //println!("Headers: {:#?}", res.headers());
            res.into_body().for_each(|chunk| {
                io::stdout().write_all(&chunk)
                    .map_err(|e| panic!("example expects stdout is open, error={}", e))
            })
        })
        .map_err(|err| {
            println!("Error {}", err);
        })
}

// https://hyper.rs/guides/client/configuration/
fn fetch_url_https(url: hyper::Uri) -> impl Future<Item=(), Error=()> {
    let https = HttpsConnector::new(4).expect("TLS initialization failed");
    let client = Client::builder()
        .build::<_, hyper::Body>(https);

    client
        .get(url)
        .and_then(|res| {
            //println!("Response: {}", res.status());
            //println!("Headers: {:#?}", res.headers());
            res.into_body().for_each(|chunk| {
                io::stdout().write_all(&chunk)
                    .map_err(|e| panic!("example expects stdout is open, error={}", e))
            })
        })
        .map_err(|err| {
            println!("Error {}", err);
        })
}