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

    #[test]
    pub fn codegen2() {
        let handle = test_harness("codegen.js", r#"
            let containerless = require("../tracing/containerless");
            containerless.listen(function(req, resp) {
                resp('Hello, world!');
            });
        "#, "");

        println!("{}", codegen(&handle));
    }

    #[test]
    pub fn multiple_callbacks_2() {
        let handle = test_harness("multiple_callbacks_2.js", r#"
            let containerless = require('../tracing/containerless');

            let foo = 'start';
            foo = 42;
            //let foo = 42;

            containerless.listen(function(req, resp) {
                //console.error('Got a response');
                let bar = foo + 1;
                resp(req);

            });
        "#, "hello
        goodbye
        hello again");

        println!("{}", codegen(&handle));

    }

}