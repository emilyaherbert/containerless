pub mod config;

#[derive(Debug, PartialEq)]
pub enum OS {
    Linux,
    Mac,
}

impl OS {
    pub fn detect() -> Self {
        use os_info::Type;
        let info = os_info::get();
        match info.os_type() {
            Type::Linux => OS::Linux,
            Type::Macos => OS::Mac,
            t => panic!("Unsupported operating system ({:?})", t),
        }
    }
}
