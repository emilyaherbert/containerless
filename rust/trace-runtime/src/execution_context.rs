use super::error::Error;
use super::type_dynamic::{Dyn, DynResult};
pub struct ExecutionContext {}

impl ExecutionContext {
    pub fn loopback<'a, T>(
        &mut self,
        _event_name: &'static str,
        _event_arg: DynResult<'a>,
        _event_clos: DynResult<'a>,
        _loopback_id: i32,
    ) -> Result<T, Error>
    where
        T: From<()>,
    {
        Ok(().into())
    }
}
