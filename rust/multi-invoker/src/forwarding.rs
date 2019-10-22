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
            OS::Linux => {
                cmd!("sudo", "iptables", "-P", "INPUT", "ACCEPT").run()
                .and_then(|_| cmd!("sudo", "iptables", "-P", "FORWARD", "ACCEPT").run())
                .and_then(|_| cmd!("sudo", "iptables", "-P", "OUTPUT", "ACCEPT").run())
                .and_then(|_| cmd!("sudo", "iptables", "-t", "nat", "-F").run())
                .and_then(|_| cmd!("sudo", "iptables", "-t", "mangle", "-F").run())
                .and_then(|_| cmd!("sudo", "iptables", "-F").run())
                .and_then(|_| cmd!("sudo", "iptables", "-X").run())
                .expect("could not flush rules");
            }
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
            OS::Linux => {
                cmd!("sudo", "iptables", "-t", "nat", "-A", "PREROUTING", "-s", "127.0.0.1", "-p", "tcp", "--dport", self.src_port.to_string(), "-j", "REDIRECT", "--to", self.dst_port.to_string())
                .run()
                .and_then(|_| cmd!("sudo", "iptables", "-t", "nat", "-A", "OUTPUT", "-s", "127.0.0.1", "-p", "tcp", "--dport", self.src_port.to_string(), "-j", "REDIRECT", "--to", self.dst_port.to_string()).run())
                .expect("Failed to reconfigure firewall");
            }
        }
    }
}
