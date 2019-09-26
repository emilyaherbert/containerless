pub struct Config {
    pub container_internal_port: usize,
    pub container_hostname: String,
    pub image_name: String,
    pub bind_port: u16,
    pub max_containers: usize,
}
