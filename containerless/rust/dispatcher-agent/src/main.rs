mod decontainerized_functions;

#[tokio::main]
async fn main() {
    dispatcher_agent_lib::main(decontainerized_functions::init()).await;
}
