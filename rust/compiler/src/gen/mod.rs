#![allow(dead_code)]
pub mod gen_types;
pub mod rust_types;

use std::{collections::HashMap, io::Write};

use crate::{
    gen::{gen_types::quote_typs, rust_types::to_rust_types},
    types::{Exp, Typ},
};

pub fn gen(exp: &mut Exp) -> HashMap<usize, Typ> {
    let rust_types = to_rust_types(exp);
    let q_types = quote_typs(&rust_types);

    let mut rs_file = std::fs::File::create("trace.rs").expect("Could not create .rs file.");
    rs_file
        .write_all(format!("{}", q_types).as_bytes())
        .expect("Could not write to file.");
    cmd!("rustfmt", "trace.rs").run().expect("rustfmt failed");
    cmd!("rustc", "--crate-type", "cdylib", "trace.rs")
        .run()
        .expect("Compiling to Rust failed.");

    return rust_types;
}
