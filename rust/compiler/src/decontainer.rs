use trace_runtime::ExecutionContext;
use trace_runtime::{Dyn, DynResult};

pub type ContainerlessFunc<'a> = fn(
    arena: &'a bumpalo::Bump,
    ec: &mut ExecutionContext<'a>,
    arg_cbid: Dyn<'a>,
    arg_cbargs: Dyn<'a>) -> DynResult<'a>;


pub trait Containerless {
    fn invoke<'a>(&self,    arena: &'a bumpalo::Bump,
    ec: &mut ExecutionContext<'a>,
    arg_cbid: Dyn<'a>,
    arg_cbargs: Dyn<'a>) -> DynResult<'a>;
}