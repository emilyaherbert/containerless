//! The runtime system used by the generated Rust code.

mod affine_ref;
mod common;
pub mod decontainer;
pub mod error;
pub mod execution_context;
pub mod type_dynamic;

pub use decontainer::*;
pub use error::*;
pub use execution_context::*;
pub use type_dynamic::*;

pub type Containerless = for<'a> fn(
    arena: &'a bumpalo::Bump,
    ec: &mut ExecutionContext<'a>,
    arg_cbid: Dyn<'a>,
    arg_cbargs: Dyn<'a>,
) -> DynResult<'a>;
