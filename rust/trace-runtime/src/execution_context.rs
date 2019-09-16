use super::error::Error;
use super::type_dynamic::{Dyn, DynResult};

use std::io::{self, Write};

use hyper::{Client, Uri};
use hyper::rt::{self, Future, Stream};
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

impl ExecutionContext {
    pub fn loopback<'a>(
        &mut self,
        event_name: &'static str,
        _event_arg: DynResult<'a>,
        _event_clos: DynResult<'a>,
        loopback_id: i32,
    ) -> DynResult<'a>
    {
        if event_name == "listen" {

            println!("something is happening at least!");

            let mut rt = Runtime::new().unwrap();
            let client = Client::new();

            /*
            let fut = client.get(Uri::from_static("http://httpbin.org/ip"))
                .and_then(|res| {
                    println!("status: {}", res.status());
                    res.into_body().concat2()
                })
                .and_then(|body| {
                    let s = ::std::str::from_utf8(&body)
                        .expect("httpbin sends utf-8 JSON");

                    println!("body: {}", s);
                    Ok(())
                })
                .map_err(|err| {
                    println!("error: {}", err);
                });
            rt::run(fut);
            */

            let mut rt = Runtime::new().unwrap();
            rt.block_on(
                fetch_url(Uri::from_static("http://httpbin.org/ip"))
                    .and_then(|res| {
                        println!("{:?}", res);
                        println!("Something else after the first thing!");
                        Ok(())
                    })
            ).unwrap();
            rt.shutdown_now().wait().expect("could not shutdown Tokio");

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
fn fetch_url(url: hyper::Uri) -> impl Future<Item=(), Error=()> {
    let client = Client::new();

    client
        // Fetch the url...
        .get(url)
        // And then, if we get a response back...
        .and_then(|res| {
            println!("Response: {}", res.status());
            println!("Headers: {:#?}", res.headers());

            // The body is a stream, and for_each returns a new Future
            // when the stream is finished, and calls the closure on
            // each chunk of the body...
            res.into_body().for_each(|chunk| {
                io::stdout().write_all(&chunk)
                    .map_err(|e| panic!("example expects stdout is open, error={}", e))
            })
        })
        // If all good, just tell the user...
        .map(|_| {
            println!("\n\nDone.");
        })
        // If there was an error, let the user know...
        .map_err(|err| {
            eprintln!("Error {}", err);
        })
}