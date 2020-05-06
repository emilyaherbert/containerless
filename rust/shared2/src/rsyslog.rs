use std::net::SocketAddr;
use syslog::Facility;
use log::LevelFilter;

pub fn init_using_env() {
    let my_ip = machine_ip::get()
        .expect("getting IP address");
    let rsyslog_addr = std::env::var("LOG_RSYSLOG_ADDR")
        .expect("LOG_RSYSLOG_ADDR environment variable is not set")
        .parse::<SocketAddr>()
        .expect("invalid value for LOG_RSYSLOG_ADDR");
    let level = std::env::var("LOG_LEVEL")
        .expect("LOG_LEVEL environment variable is not set")
        .parse::<LevelFilter>()
        .expect("invalid value for LOG_LEVEL");
    syslog::init_udp(
        format!("{}:0", my_ip).parse::<SocketAddr>().unwrap(),
        rsyslog_addr,
        "containerless".to_string(),
        Facility::LOG_USER,
        level)
        .expect("configuring rsyslog logging");
    eprintln!("Subsequent logs are now being sent to the rsyslog server at {}", rsyslog_addr);
}
