use super::types as t;

use http;
use k8s_openapi::api::apps::v1::{
    Deployment, DeploymentSpec, DeploymentStatus, ReplicaSet, ReplicaSetSpec, ReplicaSetStatus,
};
use k8s_openapi::api::core::v1::{Pod, PodSpec, PodStatus, Service, ServiceSpec, ServiceStatus};
use kube;
use kube::api::{Api, DeleteParams, ListParams, Object, PatchParams, PostParams};
use kube::config::Configuration;
use std::collections::HashMap;
//use futures::{Stream, StreamExt};

pub struct Client {
    pods: Api<Object<PodSpec, PodStatus>>,
    services: Api<Object<ServiceSpec, ServiceStatus>>,
    replica_set: Api<Object<ReplicaSetSpec, ReplicaSetStatus>>,
    deployment: Api<Object<DeploymentSpec, DeploymentStatus>>,
}

impl Client {
    pub async fn from_config(
        config: Configuration, namespace: &str,
    ) -> Result<Client, kube::Error> {
        let client = kube::client::APIClient::new(config);
        let pods = kube::api::Api::v1Pod(client.clone()).within(namespace);
        let services = kube::api::Api::v1Service(client.clone()).within(namespace);
        let replica_set = kube::api::Api::v1ReplicaSet(client.clone()).within(namespace);
        let deployment = kube::api::Api::v1Deployment(client.clone()).within(namespace);
        return Ok(Client {
            pods,
            services,
            replica_set,
            deployment,
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

    pub async fn patch_service(&self, service: Service) -> Result<(), kube::Error> {
        let name = service
            .metadata
            .as_ref()
            .expect("metadata omitted")
            .name
            .as_ref()
            .expect("metadata.name omitted");
        let params = PatchParams::default();
        let _ = self
            .services
            .patch(name, &params, serde_json::to_vec(&service)?)
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

    pub async fn list_services(&self) -> Result<Vec<t::ServiceSnapshot>, kube::Error> {
        let params = ListParams::default();
        let svcs = self.services.list(&params).await?;

        return Ok(svcs
            .items
            .into_iter()
            .map(|item| t::ServiceSnapshot {
                name: item.metadata.name,
                spec: item.spec,
            })
            .collect());
    }

    pub async fn list_services_by_label(
        &self, label: &str,
    ) -> Result<Vec<t::ServiceSnapshot>, kube::Error> {
        let params = ListParams {
            field_selector: None,
            include_uninitialized: false,
            label_selector: Some(label.to_string()),
            timeout: None,
        };
        let svcs = self.services.list(&params).await?;

        return Ok(svcs
            .items
            .into_iter()
            .map(|item| t::ServiceSnapshot {
                name: item.metadata.name,
                spec: item.spec,
            })
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

    pub async fn list_replica_sets_by_label(
        &self, label: &str,
    ) -> Result<Vec<(String, ReplicaSetSpec)>, kube::Error> {
        let params = ListParams {
            field_selector: None,
            include_uninitialized: false,
            label_selector: Some(label.to_string()),
            timeout: None,
        };
        let replica_sets = self.replica_set.list(&params).await?;
        return Ok(replica_sets
            .items
            .into_iter()
            .map(|item| (item.metadata.name, item.spec))
            .collect());
    }

    pub async fn delete_pod(&self, name: &str) -> Result<(), kube::Error> {
        let params = DeleteParams::default();
        // NOTE(emily): Maybe supposed to be a '?' here?
        let _ = self.pods.delete(name, &params).await;
        return Ok(());
    }

    pub async fn list_pods(&self) -> Result<Vec<t::PodSnapshot>, kube::Error> {
        let params = ListParams::default();
        let pods = self.pods.list(&params).await?;
        let mut snapshots = vec![];
        for item in pods.into_iter() {
            let name = item.metadata.name;
            let (phase, condition) = self.get_pod_phase_and_readiness(&name).await?;
            snapshots.push(t::PodSnapshot {
                name,
                spec: item.spec,
                phase,
                condition,
            })
        }
        Ok(snapshots)
    }

    pub async fn list_pods_by_label(
        &self, label: &str,
    ) -> Result<Vec<t::PodSnapshot>, kube::Error> {
        let params = ListParams {
            field_selector: None,
            include_uninitialized: false,
            label_selector: Some(label.to_string()),
            timeout: None,
        };
        let pods = self.pods.list(&params).await?;
        let mut snapshots = vec![];
        for item in pods.into_iter() {
            let name = item.metadata.name;
            let (phase, condition) = self.get_pod_phase_and_readiness(&name).await?;
            snapshots.push(t::PodSnapshot {
                name,
                spec: item.spec,
                phase,
                condition,
            })
        }
        Ok(snapshots)
    }

    pub async fn watch_pods_by_label(
        &self, label: &str, timeout: u32,
    ) -> Result<http::Request<Vec<u8>>, kube::Error> {
        let params = ListParams {
            field_selector: None,
            include_uninitialized: false,
            label_selector: Some(label.to_string()),
            timeout: Some(timeout),
        };
        let _what = self.pods.watch(&params, "v1").await?;

        unimplemented!()
    }

    pub async fn new_deployment(&self, deployment: Deployment) -> Result<(), kube::Error> {
        let params = PostParams::default();
        let _ = self
            .deployment
            .create(&params, serde_json::to_vec(&deployment)?)
            .await?;
        return Ok(());
    }

    pub async fn delete_deployment(&self, name: &str) -> Result<(), kube::Error> {
        let params = DeleteParams::default();
        let _ = self.deployment.delete(name, &params).await;
        return Ok(());
    }

    pub async fn patch_deployment(&self, deployment: Deployment) -> Result<(), kube::Error> {
        let name = deployment
            .metadata
            .as_ref()
            .expect("metadata omitted")
            .name
            .as_ref()
            .expect("metadata.name omitted");
        let params = PatchParams::default();
        let _ = self
            .deployment
            .patch(name, &params, serde_json::to_vec(&deployment)?)
            .await?;
        return Ok(());
    }

    pub async fn get_deployment_status(
        &self, name: &str,
    ) -> Result<t::DeploymentStatus, kube::Error> {
        let raw_status = self
            .deployment
            .get_status(name)
            .await?
            .status
            .expect("status not set");
        let status = t::DeploymentStatus {
            replicas: raw_status.replicas.expect("replicas is not set") as usize,
            observed_generation: raw_status
                .observed_generation
                .expect("observed_generation is not set") as usize,
        };
        return Ok(status);
    }

    pub async fn get_pod_phase_and_readiness(
        &self, name: &str,
    ) -> Result<(t::PodPhase, t::PodCondition), kube::Error> {
        let status = self.pods.get_status(name).await?.status;
        match status {
            None => return Ok((t::PodPhase::Unknown, t::PodCondition::Unknown)),
            Some(status) => {
                let phase = status.phase.as_deref();
                let ready = status
                    .conditions
                    .and_then(|vec| vec.into_iter().find(|condition| condition.type_ == "Ready"))
                    .map(|condition| condition.status);
                let friendly_phase = match phase {
                    None => t::PodPhase::Unknown,
                    Some("Pending") => t::PodPhase::Pending,
                    Some("Running") => t::PodPhase::Running,
                    Some("Succeeded") => t::PodPhase::Succeeded,
                    Some("Failed") => t::PodPhase::Failed,
                    Some("Unknown") => t::PodPhase::Unknown,
                    Some(phase) => panic!("unknown phase {}", phase),
                };
                let friendly_ready = match ready.as_deref() {
                    None => t::PodCondition::Unknown,
                    Some("Unknown") => t::PodCondition::Unknown,
                    Some("True") => t::PodCondition::True,
                    Some("False") => t::PodCondition::False,
                    Some(condition) => panic!("unknown condition {}", condition),
                };
                return Ok((friendly_phase, friendly_ready));
            }
        }
    }

    /// Returns a snapshot of the current system.
    pub async fn system_snapshot(&self) -> Result<t::SystemSnapshot, kube::Error> {
        let pods = self.list_pods().await?;
        let services = self.list_services().await?;
        let mut pods_hm = HashMap::new();
        for pod in pods.into_iter() {
            pods_hm.insert(pod.name.clone(), pod);
        }
        let mut services_hm = HashMap::new();
        for service in services.into_iter() {
            services_hm.insert(service.name.clone(), service);
        }
        Ok(t::SystemSnapshot {
            pods: pods_hm,
            services: services_hm,
        })
    }

    pub async fn list_pods_by_label_and_field(
        &self, label: String, field: &str,
    ) -> Result<Vec<t::PodSnapshot>, kube::Error> {
        let params = ListParams {
            field_selector: Some(field.to_string()),
            include_uninitialized: false,
            label_selector: Some(label),
            timeout: None,
        };
        let pods = self.pods.list(&params).await?;
        let mut snapshots = vec![];
        for item in pods.into_iter() {
            let name = item.metadata.name;
            let (phase, condition) = self.get_pod_phase_and_readiness(&name).await?;
            snapshots.push(t::PodSnapshot {
                name,
                spec: item.spec,
                phase,
                condition,
            })
        }
        Ok(snapshots)
    }
}
