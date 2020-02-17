mod state;
mod types;

use futures::Stream;
use http;
use hyper::service::{make_service_fn, service_fn};
use hyper::{Body, Client, Server};
use kube::{self, api::PostParams};
use serde_json::json;
use state::State;
use std::convert::Infallible;
use tokio::prelude::*;
use types::*;
use warp::Filter;

async fn get_kube_client() -> Result<usize, kube::Error> {
    let config = kube::config::incluster_config()?;
    let client = kube::client::APIClient::new(config);
    // let pods = kube::api::Api::v1Pod(client.clone()).within("default");
    // let pod_list = pods.list(&kube::api::ListParams::default()).await?;
    // println!("{}", pod_list.items.len());
    // let p = pods.get("blog").await?;
    let services = kube::api::Api::v1Service(client.clone()).within("default");
    let req = json!({
        "apiVersion": "v1",
        "kind": "Service",
        "metadata": {
          "name": "test"
        },
        "spec": {
          "selector": {
            "app": "function-storage"
          },
          "ports": [
            { "name": "http", "port": 8080 }
          ]
        }
    });
    services
        .create(&PostParams::default(), serde_json::to_vec(&req)?)
        .await?;

    // let replication_controller =
    //     kube::api::Api::v1ReplicationController(client.clone()).within("default");
    // let r = replication_controller.create(0, 0).await?;
    return Ok(34);
}

// async fn handle_request<S, B>(
//     function_name: String,
//     path: String,
//     method: warp::http::Method,
//     body: S,
//     state: State,
// ) -> Result<impl warp::Reply, warp::Rejection>
// where
//     // B: bytes::buf::Buf,
//     S: Stream<Item = Result<B, warp::Error>> + 'static
// {
//     let client = hyper::Client::new();
//     let req = hyper::Request::builder()
//       .uri(format!("http://{}/{}", &function_name, &path))
//       .body(body)
//       .unwrap();
//     let resp = client.request(req).await;

//     return Ok("ok");
// }

async fn handle_req(state: State, req: Request) -> Result<Response, hyper::Error> {
    let (parts, body) = req.into_parts();
    let path = parts.uri.path();
    let mut split_path = path.splitn(2, '/');
    let function_name = split_path.next().unwrap();
    let function_path = split_path.next().unwrap();

    return state
        .invoke(function_name, function_path, parts.method, body)
        .await;
}

#[tokio::main]
async fn main() {
    if let Err(e) = get_kube_client().await {
        println!("{:?}", e);
    }

    let state = State::new(vec![]);

    let make_svc = make_service_fn(move |_| {
        let state = state.clone();
        futures::future::ok::<_, Infallible>(service_fn(move |req| handle_req(state.clone(), req)))
    });

    let addr = ([0, 0, 0, 0], 8080).into();
    Server::bind(&addr)
        .serve(make_svc)
        .await
        .expect("could not start server");
    return;
}
