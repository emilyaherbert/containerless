use dispatcher_agent_lib::trace_runtime::Containerless;
use std::collections::HashMap;
pub fn init() -> HashMap<&'static str, Containerless> {
    let ht: HashMap<&'static str, Containerless> = HashMap::new();
    return ht;
} 