use bumpalo::Bump;
use super::error::Error;
use super::type_dynamic::{Dyn, DynResult};

pub struct ExecutionContext {
    pub events: Vec<(String, i32)>,
 }

impl ExecutionContext {
    pub fn loopback<'a>(
        &mut self,
        event_name: &'static str,
        _event_arg: Dyn<'a>,
        _event_clos: Dyn<'a>,
        loopback_id: i32,
    ) -> DynResult<'a>
    {
        if event_name == "listen" {
            self.events.push((event_name.to_string(), loopback_id));
            Ok(Dyn::int(0))

        }
        else {
            Ok(Dyn::int(0))
        }
    }

    pub fn send<'a>(&mut self, _value: Dyn<'a>) -> DynResult<'a> {
        Ok(Dyn::int(0))
    }

    pub fn new() -> ExecutionContext {
        ExecutionContext {
            events: Vec::new()
        }
    }
}
