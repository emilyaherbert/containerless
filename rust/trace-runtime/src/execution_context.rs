use super::error::Error;
use super::type_dynamic::{Dyn, DynResult};
pub struct ExecutionContext {}

impl ExecutionContext {
    pub fn loopback<'a>(
        &mut self,
        _event_name: &'static str,
        _event_arg: DynResult<'a>,
        _event_clos: DynResult<'a>,
        _loopback_id: i32,
    ) -> DynResult<'a>
    {
        Dyn::int(0)
    }

    pub fn send<'a>(&mut self, _value: Dyn<'a>) -> DynResult<'a> {
        Dyn::int(0)
    }
}
