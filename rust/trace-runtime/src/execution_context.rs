// use super::error::Error;
use super::type_dynamic::{Dyn, DynResult};
use std::convert::TryInto;

/// An enumeration of the events that a decontainerized function can run
/// within Rust.
pub enum AsyncOp {
    /// This event immediately completes, which is useful for testing without
    /// performing real I/O.
    Immediate,
    Listen,
    Request(String),
}

/// The execution context allows a callback to send new events. The lifetime
/// `'a` is the lifetime of the arena in which the function may allocate
/// heap values.
pub struct ExecutionContext<'a> {
    pub new_ops: Vec<(AsyncOp, usize, Dyn<'a>)>,
}

impl<'a> ExecutionContext<'a> {
    /// Send a new event. The argument `indicator` is sent back with the
    /// response, which helps the decontainerized keep track of multiple
    /// pending requests. There is no requirement that indicators be distinct,
    /// but that will be helpful to client code.
    pub fn loopback_int(&mut self,
        op: AsyncOp,
        indicator: usize,
        closure: Dyn<'a>) {
        self.new_ops.push((op, indicator, closure));
    }

    pub fn loopback(
        &mut self,
        event_name: &'static str,
        _event_arg: Dyn<'a>,
        event_clos: Dyn<'a>,
        indicator: i32,
    ) -> DynResult<'a>
    {
        if event_name == "listen" {
            self.loopback_int(AsyncOp::Listen, (indicator as i32).try_into().unwrap(), event_clos);
            Ok(Dyn::int(0))
        }
        else {
            Ok(Dyn::int(0))
        }
    }

    pub fn send(&mut self, _value: Dyn<'a>) -> DynResult<'a> {
        Ok(Dyn::int(0))
    }

}