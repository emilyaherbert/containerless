
pub mod rust_types;

use std::collections::HashMap;

use crate::{
    types::{
        Exp,
        Typ
    },
    gen::rust_types::to_rust_types
};

pub fn gen(exp: &mut Exp) -> HashMap<usize, Typ> {
    return to_rust_types(exp);
}