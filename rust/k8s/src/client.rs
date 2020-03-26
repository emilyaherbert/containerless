use k8s_openapi::api::apps::v1::{ReplicaSet, ReplicaSetSpec, ReplicaSetStatus};
use k8s_openapi::api::core::v1::{Pod, PodSpec, PodStatus, Service, ServiceSpec, ServiceStatus};
use kube;
use kube::api::{Api, DeleteParams, ListParams, Object, PatchParams, PostParams};
use kube::config::Configuration;

pub struct Client {
    pods: Api<Object<PodSpec, PodStatus>>,
    services: Api<Object<ServiceSpec, ServiceStatus>>,
    replica_set: Api<Object<ReplicaSetSpec, ReplicaSetStatus>>,
}

impl Client {
    pub async fn from_config(
        config: Configuration,
        namespace: &str,
    ) -> Result<Client, kube::Error> {
        let client = kube::client::APIClient::new(config);
        let pods = kube::api::Api::v1Pod(client.clone()).within(namespace);
        let services = kube::api::Api::v1Service(client.clone()).within(namespace);
        let replica_set = kube::api::Api::v1ReplicaSet(client.clone()).within(namespace);
        return Ok(Client {
            pods,
            services,
            replica_set,
        });
    }

    pub async fn from_kubeconfig_file(namespace: &str) -> Result<Client, kube::Error> {
        let config = kube::config::load_kube_config().await.unwrap();
        return Self::from_config(config, namespace).await;
    }

    /// Creates a new Client that only works within pods deployed on the k8s
    /// cluster.
    pub async fn new(namespace: &str) -> Result<Client, kube::Error> {
        let config = kube::config::incluster_config()?;
        return Self::from_config(config, namespace).await;
    }

    pub async fn new_pod(&self, pod: Pod) -> Result<(), kube::Error> {
        let params = PostParams::default();
        let _ = self.pods.create(&params, serde_json::to_vec(&pod)?).await?;
        return Ok(());
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
        let name = replica_set
            .metadata
            .as_ref()
            .expect("metadata omitted")
            .name
            .as_ref()
            .expect("metadata.name omitted");
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

    pub async fn list_services(&self) -> Result<Vec<(String, ServiceSpec)>, kube::Error> {
        let params = ListParams::default();
        let svcs = self.services.list(&params).await?;

        return Ok(svcs
            .items
            .into_iter()
            .map(|item| (item.metadata.name, item.spec))
            .collect());
    }

    pub async fn list_replica_sets(&self) -> Result<Vec<(String, ReplicaSetSpec)>, kube::Error> {
        let params = ListParams::default();
        let replica_sets = self.replica_set.list(&params).await?;
        return Ok(replica_sets
            .items
            .into_iter()
            .map(|item| (item.metadata.name, item.spec))
            .collect());
    }

    pub async fn delete_pod(&self, name: &str) -> Result<(), kube::Error> {
        let params = DeleteParams::default();
        let _ = self.pods.delete(name, &params).await;
        return Ok(());
    }
}
