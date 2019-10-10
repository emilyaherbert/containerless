use super::trace_runtime::Containerless;

#[derive(Clone)]
pub struct Config {
    pub container_internal_port: usize,
    pub container_hostname: String,
    pub image_name: String,
    pub bind_port: u16,
    pub max_containers: usize,
    pub max_container_buffer_delay: usize,
    pub min_container_lifespan: u64,
    pub cpus: String,   // string passed to Docker's --cpu flag
    pub memory: String, // string passed to Docker's -m flag
    pub utilization_log: String,
    pub containerless: Option<Containerless>,
}

impl Config {

    pub fn to_args(&self) -> [String;19] {
        return [
            self.image_name.to_string(),
            "--container-internal-port".to_string(),
            self.container_internal_port.to_string(),
            "--container-hostname".to_string(),
            self.container_hostname.to_string(),
            "--bind-port".to_string(),
            self.bind_port.to_string(),
            "--max-containers".to_string(),
            self.max_containers.to_string(),
            "--max-container-buffer-delay".to_string(),
            self.max_container_buffer_delay.to_string(),
            "--min-container-lifespan".to_string(),
            self.min_container_lifespan.to_string(),
            "--cpus".to_string(),
            self.cpus.to_string(),
            "--memory".to_string(),
            self.memory.to_string(),
            "--utilization-log".to_string(),
            self.utilization_log.to_string()
        ];
    }
}
