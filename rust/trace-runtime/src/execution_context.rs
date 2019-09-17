// use super::error::Error;
use super::type_dynamic::{Dyn};

/// An enumeration of the events that a decontainerized function can run
/// within Rust.
pub enum AsyncOp {
    /// This event immediately completes, which is useful for testing without
    /// performing real I/O.
    Immediate,
    Request(String),
}


/// The execution context allows a callback to send new events.
pub struct ExecutionContext<'a> {
    pub new_ops: Vec<(AsyncOp, usize, Dyn<'a>)>,
}

impl<'a> ExecutionContext<'a> {
    /// Send a new event. The argument `indicator` is sent back with the
    /// response, which helps the decontainerized keep track of multiple
    /// pending requests. There is no requirement that indicators be distinct,
    /// but that will be helpful to client code.
    pub fn loopback(&mut self,
        op: AsyncOp,
        indicator: usize,
        closure: Dyn<'a>) {
        self.new_ops.push((op, indicator, closure));
    }

}

/*
impl ExecutionContext {
    pub fn loopback<'a>(
        &mut self,
        event_name: &'static str,
        _event_arg: DynResult<'a>,
        _event_clos: DynResult<'a>,
        loopback_id: i32,
    ) -> DynResult<'a>
    {
        if event_name == "listen" {
            self.events.push((event_name.to_string(), loopback_id));
            Dyn::int(0)

        }
        else {
            Dyn::int(0)
        }
    }

    pub fn send<'a>(&mut self, _value: Dyn<'a>) -> DynResult<'a> {
        Dyn::int(0)
    }

    pub fn new() -> ExecutionContext {
        ExecutionContext {
            events: Vec::new()
        }
    }
}
*/