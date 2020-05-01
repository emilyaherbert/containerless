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