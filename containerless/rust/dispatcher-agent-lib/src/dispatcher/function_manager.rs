use super::function_table::FunctionTable;
use super::serverless_request::*;
use super::state::{CreateMode, State};
use super::types::*;
use super::util;

use futures::prelude::*;
use tokio::task;

#[derive(Clone)]
pub struct FunctionManager {
    send_requests: mpsc::Sender<Message>,
    state: Arc<State>,
}

impl FunctionManager {
    pub async fn new(
        k8s_client: K8sClient,
        http_client: HttpClient,
        short_deadline_http_client: HttpClient,
        function_table: Weak<FunctionTable>,
        name: String, create_mode: CreateMode,
        containerless: Option<Containerless>,
        upgrade_pending: Arc<AtomicBool>,
    ) -> FunctionManager {
        let (send_requests, recv_requests) = mpsc::channel(1);
        let err_msg = format!("error raised by task for {}", &name);
        let state = State::new(name, k8s_client, http_client, short_deadline_http_client);
        task::spawn(util::log_error(
            State::function_manager_task(
                Arc::clone(&state),
                recv_requests,
                function_table,
                create_mode,
                containerless,
                upgrade_pending,
            ),
            err_msg,
        ));
        let fm = FunctionManager {
            send_requests,
            state,
        };
        return fm;
    }

    pub async fn shutdown(&mut self) -> Response {
        let (send, recv) = oneshot::channel();
        self.send_requests
            .send(Message::Shutdown(send))
            .await
            .unwrap();
        match recv.await {
            Err(err) => {
                return util::text_response(
                    500,
                    format!(
                        "(function manager shutdown early) dispatcher could not shut down function instances for {}: {:?}",
                        self.state.name, err
                    ),
                );
            }
            Ok(Err(err)) => {
                return util::text_response(
                    500,
                    format!(
                        "dispatcher could not shut down function instances for {}: {:?}",
                        self.state.name, err
                    ),
                );
            }
            Ok(Ok(())) => {
                return util::text_response(
                    200,
                    format!("All function instances shut down for {}.", self.state.name),
                );
            }
        }
    }

    pub async fn orphan(mut self) {
        self.send_requests
            .send(Message::Orphan)
            .await
            .unwrap_or_else(|_| panic!("error sending Message::Orphan for {}", self.state.name));
    }

    pub async fn invoke(
        &mut self, method: http::Method, path_and_query: &str, body: hyper::Body,
    ) -> Result<Response, hyper::Error> {
        let (send_resp, recv_resp) = oneshot::channel();
        let req = ServerlessRequest {
            payload: RequestPayload {
                method,
                path_and_query: String::from(path_and_query),
                body,
            },
            send: send_resp,
        };
        info!(target: "dispatcher", "INVOKE {}: sending request with path {} to FMT", self.state.name, path_and_query);
        self.send_requests
            .send(Message::Request(req))
            .await
            .unwrap();
        match recv_resp.await {
            Ok(result) => {
                return result;
            }
            Err(futures::channel::oneshot::Canceled) => {
                error!(target: "dispatcher", "INVOKE {}: function pods shut down before request could be made", self.state.name);
                return Ok(hyper::Response::builder()
                    .status(500)
                    .body(hyper::Body::from("dispatcher shutdown"))
                    .unwrap());
            }
        }
    }

    pub async fn extract_and_compile(&mut self) -> Response {
        let (send_resp, recv_resp) = oneshot::channel();
        self.send_requests
            .send(Message::ExtractAndCompile(send_resp))
            .await
            .unwrap();
        match recv_resp.await {
            Ok(resp) => {
                return resp;
            }
            Err(oneshot::Canceled) => {
                return util::text_response(
                    500,
                    format!(
                        "dispatcher shutdown before trace could be extracted for {}",
                        self.state.name
                    ),
                );
            }
        }
    }

    pub async fn get_mode(&mut self) -> Response {
        let (send_resp, recv_resp) = oneshot::channel();
        self.send_requests
            .send(Message::GetMode(send_resp))
            .await
            .unwrap();
        match recv_resp.await {
            Ok(resp) => {
                return resp;
            }
            Err(oneshot::Canceled) => {
                return util::text_response(
                    500,
                    format!(
                        "dispatcher shutdown before mode could be queried for {}",
                        self.state.name
                    ),
                );
            }
        }
    }
}
