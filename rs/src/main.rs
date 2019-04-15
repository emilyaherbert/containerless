#![macro_use]
extern crate quote;
extern crate syn;
extern crate serde;
extern crate serde_json;
extern crate libloading;
#[macro_use]
extern crate duct;
mod types;

use types::*;
use quote::*;
use quote::__rt::TokenStream;
use proc_macro2::Span;

fn compile_op(op: &BinOp) -> TokenStream {
    match op {
        BinOp::Add => quote! { + },
        BinOp::Mul => quote! { * }
    }
}

fn compile_exp(e: &types::Exp) -> quote::__rt::TokenStream {
    match e {
        Exp::Identifier { name } => {
            // NOTE(arjun): Writing quote! { #x } would treat x as a string and
            // generate the Rust code "x". This code is based on the following
            // example:
            // https://docs.rs/quote/0.6.12/quote/macro.quote.html#constructing-identifiers
            let q_id = syn::Ident::new(&format!("x{}", name), Span::call_site());
            quote! { #q_id }
        },
        Exp::Number { value } => quote! { #value },
        Exp::String { value } => quote! { #value },
        Exp::BinOp { op, e1, e2 } => {
            let q_op = compile_op(op);
            let q_e1 = compile_exp(&e1);
            let q_e2 = compile_exp(&e2);
            quote! { #q_e1 #q_op #q_e2 }
        },
        _ => panic!("wtf")
    }
}

fn compile_stmt(stmt: &types::Stmt) -> quote::__rt::TokenStream {
    match stmt {
        Stmt::Let { name, body } => {
            let q_body = compile_exp(body);
            let q_name = syn::Ident::new(&format!("x{}", name), Span::call_site());
            quote! {
                let #q_name = #q_body;
            }
        },
        Stmt::Block { body } => {
            let q_body = body.iter().map(|s| compile_stmt(s));
            quote! {
                #(#q_body)*
            }
        },
        Stmt::Return { value } => {
            let q_value = compile_exp(value);
            quote! {
                return #q_value;
            }
        },
        _ => panic!("NYI")
    }

}

fn compile_program(stmt: &Stmt)-> quote::__rt::TokenStream {
    let q_stmt = compile_stmt(stmt);
    quote! {
        #[no_mangle]
        pub extern "C" fn trace_main() -> f64 {
            #q_stmt
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_run(src_json: &str) -> f64 {
        use std::io::Read;
        use std::io::Write;
        let mut file = std::fs::File::open(src_json)
            .expect("JSON file not found");
        let mut json_string = String::new();
        file.read_to_string(&mut json_string).expect("file read failed");
        let stmt: Stmt = serde_json::from_str(&json_string)
            .expect("failed to parse JSON");
        let rust_code = compile_program(&stmt);
        let mut rs_file = std::fs::File::create("trace.rs")
            .expect("could not create .rs file");
        rs_file.write_all(format!("{}", rust_code).as_bytes())
            .expect("could not write write file");
        cmd!("rustc", "--crate-type", "cdylib", "trace.rs").run()
            .expect("Compiling Rust failed");
        let lib = libloading::Library::new("libtrace.dylib")
            .expect("Loading dylib failed");
        unsafe {
            let fun: libloading::Symbol<unsafe extern fn() -> f64> =
                lib.get(b"trace_main")
                    .expect("symbol trace_main not found");
            return fun();
        }
    }

    #[test]
    fn test_loading() {
        assert_eq!(test_run("example.json"), 12.0);
    }

}
fn main() {
    use std::io::Read;
    let args: Vec<_> = std::env::args().collect();
    let mut file = std::fs::File::open(&args[1]).expect("file not found");
    let mut json_string = String::new();
    file.read_to_string(&mut json_string).expect("file read failed");
    let stmt: Stmt = serde_json::from_str(&json_string).expect("failed to parse JSON");
    let s = compile_program(&stmt);
    println!("{}", s);
}
