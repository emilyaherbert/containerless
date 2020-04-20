mod error;
mod types;
mod assertions;
mod transformer;
mod lift_callbacks;
mod rustify;
mod codegen;

use types::Exp;
use assertions::Assertions;
use transformer::Transformer;
use lift_callbacks::LiftCallbacks;
use rustify::Rustify;

pub fn compile(
    serverless_function_name: String,
    dest_file: &str,
    trace: &str) -> Result<(), error::Error> {
    let exp = serde_json::from_str::<Exp>(trace)?;
    let mut assertions = Assertions::new();
    assertions.assert_supposed_grammar(&exp);
    assertions.assert_unique_names(&exp);
    assertions.assert_all_options_are_none(&exp);
    let mut transformer = Transformer::new();
    let exp2 = transformer.transform(&exp);
    let mut lift_callbacks = LiftCallbacks::new();
    let mut exp3 = lift_callbacks.lift(&exp2);
    let mut rustify = Rustify::new();
    rustify.rustify(&mut exp3);
    codegen::codegen(&exp3, dest_file);
    return Ok(());
}