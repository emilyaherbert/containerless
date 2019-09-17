pub mod error;
pub mod execution_context;
pub mod type_dynamic;
pub mod decontainer;

pub use error::*;
pub use execution_context::*;
pub use type_dynamic::*;
pub use decontainer::*;

pub trait Containerless {
    fn invoke<'a>(&self,    arena: &'a bumpalo::Bump,
    ec: &mut ExecutionContext<'a>,
    arg_cbid: Dyn<'a>,
    arg_cbargs: Dyn<'a>) -> DynResult<'a>;
}