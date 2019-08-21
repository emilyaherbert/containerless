pub mod rust_types;
pub mod gen_types;

use std::{
    io::Write,
    collections::HashMap
};

use crate::{
    types::{
        Exp,
        Typ
    },
    gen::{
        rust_types::to_rust_types,
        gen_types::compile_typs
    }
};

pub fn gen(exp: &mut Exp) -> HashMap<usize, Typ> {
    let rust_types = to_rust_types(exp);
    let q_types = compile_typs(&rust_types);

    let mut rs_file = std::fs::File::create("trace.rs")
        .expect("Could not create .rs file.");
    rs_file.write_all(format!("{}", q_types).as_bytes())
        .expect("Could not write to file.");
    cmd!("rustc", "--crate-type", "cdylib", "trace.rs").run()
        .expect("Compiling to Rust failed.");

    return rust_types;
}