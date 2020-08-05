use k8s_openapi::api::core::v1::{PodSpec, ServiceSpec};

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum PodPhase {
    Pending,
    Running,
    Succeeded,
    Failed,
    Unknown,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum PodCondition {
    True,
    False,
    Unknown,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DeploymentStatus {
    pub replicas: usize,
    pub observed_generation: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PodSnapshot {
    pub name: String,
    pub spec: PodSpec,
    pub phase: PodPhase,
    pub condition: PodCondition
}

#[derive(Debug, Clone, PartialEq)]
pub struct ServiceSnapshot {
    pub name: String,
    pub spec: ServiceSpec
}

#[derive(Debug, Clone, PartialEq)]
pub struct SystemSnapshot {
    pub pods: Vec<PodSnapshot>,
    pub services: Vec<ServiceSnapshot>

}