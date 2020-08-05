use k8s_openapi::api::core::v1::{PodSpec, ServiceSpec};
use std::collections::HashMap;
use std::fmt;

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
    pub pods: HashMap<String, PodSnapshot>,
    pub services: HashMap<String, ServiceSnapshot>
}

pub struct SystemStatus {
    pub controller_service: bool,
    pub dispatcher_pod: bool,
    pub dispatcher_service: bool,
    pub storage_pod: bool,
    pub storage_service: bool,
    pub function_pods: HashMap<String, bool>,
    pub function_services: HashMap<String, bool>
}

impl fmt::Display for SystemStatus {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut output_vec: Vec<String> = vec!();

        output_vec.push("controller:".to_string());
        if self.controller_service {
            output_vec.push("\t✓\t\tservice".to_string());
        } else {
            output_vec.push("\tX\t\tservice".to_string());
        }

        output_vec.push("\ndispatcher:".to_string());
        if self.dispatcher_service {
            output_vec.push("\t✓\t\tservice".to_string());
        } else {
            output_vec.push("\tX\t\tservice".to_string());
        }
        if self.dispatcher_pod {
            output_vec.push("\t✓\t\tpod".to_string());
        } else {
            output_vec.push("\tX\t\tpod".to_string());
        }

        output_vec.push("\nstorage:".to_string());
        if self.storage_service {
            output_vec.push("\t✓\t\tservice".to_string());
        } else {
            output_vec.push("\tX\t\tservice".to_string());
        }
        if self.storage_pod {
            output_vec.push("\t✓\t\tpod".to_string());
        } else {
            output_vec.push("\tX\t\tpod".to_string());
        }

        output_vec.push("\nTODO: function status stuff!".to_string());
    
        let output = output_vec.join("\n");
        write!(f, "{}", output)
    }
}