#![allow(dead_code)]

#[macro_use]
extern crate duct;

mod types;
mod verif;
mod gen;

fn main() {
    println!("Hello world!");
}

#[cfg(test)]
mod tests {

    use crate::{
        types::{
            Exp,
            to_exp
        },
        verif::{
            verify,
            codegen::codegen
        },
        gen::gen
    };

    fn test_harness(filename: &str, code: &str, requests: &str) -> Exp {
        // Turns a code string to Exp tree
        let exp = to_exp(filename, code, requests);
        // Checks assertions, calculates types
        let mut exp2 = verify(&exp);
        // Translates typ's to RustType's, creates a usize -> Typ
        let rust_types = gen(&mut exp2);

        println!("{:?}", rust_types);
        //println!("{:#?}", exp2);

        return exp2;
    }

}