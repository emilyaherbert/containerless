use serde::{Serialize,Deserialize};

#[derive(Debug, Serialize, Deserialize)]
pub enum InitialState {
    Tracing,
    Decontainerized
}

#[derive(Debug, Serialize, Deserialize)]
pub struct InvokerConfig {
    pub image_name: String,
    #[serde(default = "InvokerConfig::default_initial_state")]
    pub initial_state: InitialState,
    #[serde(default = "InvokerConfig::default_container_internal_port")]
    pub container_internal_port: usize,
    #[serde(default = "InvokerConfig::default_container_hostname")]
    pub container_hostname: String,
    #[serde(default = "InvokerConfig::default_bind_port")]
    pub bind_port: u16,
    #[serde(default = "InvokerConfig::default_max_containers")]
    pub max_containers: usize,
    #[serde(default = "InvokerConfig::default_max_container_buffer_delay")]
    pub max_container_buffer_delay: usize,
    #[serde(default = "InvokerConfig::default_min_container_lifespan")]
    pub min_container_lifespan: u64,
    #[serde(default = "InvokerConfig::default_cpus")]
    pub cpus: String,   // string passed to Docker's --cpu flag
    #[serde(default = "InvokerConfig::default_memory")]
    pub memory: String, // string passed to Docker's -m flag
    #[serde(default = "InvokerConfig::default_utilization_log")]
    pub utilization_log: String
}

impl InvokerConfig {

    pub fn from_string(s: &str) -> Self {
        serde_json::from_str(s)
            .unwrap_or_else(|e| 
                panic!("Error deserializing InvokerConfig. Error: {:?}. JSON string: {:?}", e, &s))
    }

    pub fn to_string(&self) -> String {
        serde_json::to_string(self)
            .expect("could not serialize InvokerConfig")
    }

    fn default_initial_state() -> InitialState {
        InitialState::Tracing
    }

    fn default_container_hostname() -> String {
        "localhost".to_string()
    }

    fn default_container_internal_port() -> usize {
        3000
    }

    fn default_bind_port() -> u16 {
        8080
    }

    fn default_max_containers() -> usize {
        4
    }

    fn default_max_container_buffer_delay() -> usize {
        100
    }

    fn default_min_container_lifespan() -> u64 {
        10
    }

    fn default_cpus() -> String {
        "1.0".to_string()
    }

    fn default_memory() -> String {
        "512MB".to_string()
    }

    fn default_utilization_log() -> String {
        "utilization.log".to_string()
    }


}