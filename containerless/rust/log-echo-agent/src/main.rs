// Boilerplate that is always needed
use swagger::{ApiError, EmptyContext, Has, XSpanIdString};
use async_trait::async_trait;
// The types that actually matter to us.
use log_openapi::{context::MakeAddContext, server::MakeService, Api, LogPostResponse};
use std::marker::PhantomData;
use std::net::SocketAddr;

// No idea why the C type argument is needed
#[derive(Copy, Clone)]
struct Server<C> {
    marker: PhantomData<C>,
}

// This is a hack to support async functions in traits. It is quite widely used and I suspect that
// async functions in traits will eventually be standardized.
#[async_trait]
impl<C> Api<C> for Server<C>
where
    C: Has<XSpanIdString> + Send + Sync,
{
    async fn log_post(&self, body: String, _context: &C) -> Result<LogPostResponse, ApiError> {
        println!("{}", body);
        return Ok(LogPostResponse::OK);
    }
}

#[tokio::main]
async fn main() {
    // Boilerplate that is necessary to turn the Server { ... } into a service for Hyper.
    let service = MakeService::new(Server {
        marker: PhantomData,
    });
    let service = MakeAddContext::<_, EmptyContext>::new(service);

    let addr: SocketAddr = "0.0.0.0:80".parse()
        .expect("failed to listen on port 80");

    // Conventional hyper server setup
    hyper::server::Server::bind(&addr)
        .serve(service)
        .await
        .expect("error while running web server");
}
