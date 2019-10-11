use duct::cmd;
use shared::OS;

pub struct Forwarding {
    src_port: u16,
    dst_port: u16,
    os: shared::OS,
}

impl Drop for Forwarding {
    fn drop(&mut self) {
        match self.os {
            OS::Mac => {
                cmd!("sudo", "pfctl", "-F", "all", "-f", "/etc/pf.conf")
                .run()
                .expect("could not flush rules");
            }
            OS::Linux => unimplemented!()
        }
    }
}

impl Forwarding {
    pub fn new(src_port: u16, dst_port: u16) -> Self {
        let os = OS::detect();
        let mut result = Forwarding {
            src_port,
            dst_port,
            os,
        };
        result.do_it();
        return result;
    }

    pub fn change_destination(&mut self, dst_port: u16) {
        self.dst_port = dst_port;
        self.do_it();
    }

    fn do_it(&mut self) {
        match self.os {
            OS::Mac => {
                cmd!("sudo", "pfctl", "-f", "-")
                .stdin_bytes(format!(
                    "rdr pass inet proto tcp from any to any port {} -> 127.0.0.1 port {}\n",
                    self.src_port, self.dst_port
                ))
                .run()
                .expect("Failed to reconfigure firewall");
            }
            OS::Linux => unimplemented!()
        }
    }
}
