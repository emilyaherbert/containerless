use duct::cmd;
use shared::OS;

enum LinuxState {
    First,
    Alive
}

pub struct Forwarding {
    src_port: u16,
    dst_port: u16,
    os: shared::OS,
    linux_state: LinuxState,
    prev_dst_port: u16
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
                match self.linux_state {
                    LinuxState::Alive => {
                        cmd!("sudo", "iptables", "-t", "nat", "-D", "PREROUTING", "-p", "tcp", "--dport", self.src_port.to_string(), "-j", "REDIRECT", "--to", self.dst_port.to_string())
                        .run()
                        .expect("could not flush rules");
                    },
                    LinuxState::First => panic!("Did not expect to see this here!")
                }
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
            linux_state: LinuxState::First,
            prev_dst_port: dst_port
        };
        result.do_it();
        return result;
    }

    pub fn change_destination(&mut self, dst_port: u16) {
        self.prev_dst_port = self.dst_port.clone();
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
                match self.linux_state {
                    LinuxState::First => {
                        cmd!("sudo", "iptables", "-t", "nat", "-A", "PREROUTING", "-p", "tcp", "--dport", self.src_port.to_string(), "-j", "REDIRECT", "--to", self.dst_port.to_string())
                        .run()
                        .expect("Failed to reconfigure firewall");
                        self.linux_state = LinuxState::Alive;
                    },
                    LinuxState::Alive => {
                        cmd!("sudo", "iptables", "-t", "nat", "-A", "PREROUTING", "-p", "tcp", "--dport", self.src_port.to_string(), "-j", "REDIRECT", "--to", self.dst_port.to_string())
                        .run()
                        .and_then(|_| {
                            cmd!("sudo", "iptables", "-t", "nat", "-D", "PREROUTING", "-p", "tcp", "--dport", self.src_port.to_string(), "-j", "REDIRECT", "--to", self.prev_dst_port.to_string())
                            .run()
                        })
                        .expect("Failed to reconfigure firewall");
                    }
                }
            }
        }
    }
}
