use k8s_openapi::api::apps::v1::{ReplicaSet, ReplicaSetSpec, ReplicaSetStatus};
use k8s_openapi::api::core::v1::{Service, ServiceSpec, ServiceStatus};
use kube;
use kube::config::Configuration;
use kube::api::{Api, DeleteParams, Object, PostParams, PatchParams};

pub struct Client {
    services: Api<Object<ServiceSpec, ServiceStatus>>,
    replica_set: Api<Object<ReplicaSetSpec, ReplicaSetStatus>>,
}

impl Client {

    pub async fn from_config(config: Configuration) -> Result<Client, kube::Error> {
        let client = kube::client::APIClient::new(config);
        let services = kube::api::Api::v1Service(client.clone()).within("default");
        let replica_set = kube::api::Api::v1ReplicaSet(client.clone()).within("default");
        return Ok(Client {
            services,
            replica_set,
        });
    }

    pub async fn from_kubeconfig_file() -> Result<Client, kube::Error> {
        let config = kube::config::load_kube_config().await.unwrap();
        return Self::from_config(config).await;
    }

    /// Creates a new Client that 1) interacts with resources in the default
    /// namespace, and 2) only works within pods deployed on the k8s cluster.
    pub async fn new() -> Result<Client, kube::Error> {
        let config = kube::config::incluster_config()?;
        return Self::from_config(config).await;
    }

    pub async fn new_service(&self, service: Service) -> Result<(), kube::Error> {
        let params = PostParams::default();
        let _ = self
            .services
            .create(&params, serde_json::to_vec(&service)?)
            .await?;
        return Ok(());
    }

    pub async fn new_replica_set(&self, replica_set: ReplicaSet) -> Result<(), kube::Error> {
        let params = PostParams::default();
        let _ = self
            .replica_set
            .create(&params, serde_json::to_vec(&replica_set)?)
            .await?;
        return Ok(());
    }

    pub async fn patch_replica_set(&self, replica_set: ReplicaSet) -> Result<(), kube::Error> {
        let name = replica_set.metadata.as_ref().expect("metadata omitted")
            .name.as_ref().expect("metadata.name omitted");
        let params = PatchParams::default();
        let _ = self
            .replica_set
            .patch(name, &params, serde_json::to_vec(&replica_set)?)
            .await?;
        return Ok(());
    }

    pub async fn delete_service(&self, name: &str) -> Result<(), kube::Error> {
        let params = DeleteParams::default();
        let _ = self.services.delete(name, &params).await;
        return Ok(());
    }

    pub async fn delete_replica_set(&self, name: &str) -> Result<(), kube::Error> {
        let params = DeleteParams::default();
        let _ = self.replica_set.delete(name, &params).await;
        return Ok(());
    }
}
