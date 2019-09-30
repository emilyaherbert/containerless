pub struct Config {
    pub container_internal_port: usize,
    pub container_hostname: String,
    pub image_name: String,
    pub bind_port: u16,
    pub max_containers: usize,
    pub max_container_buffer_delay: usize,
    pub min_container_lifespan: u64,
    pub cpus: String, // string passed to Docker's --cpu flag
    pub memory: String // string passed to Docker's -m flag
}
