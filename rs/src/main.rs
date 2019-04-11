#![macro_use]
extern crate quote;
extern crate syn;
extern crate serde;
extern crate serde_json;
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

fn compile(e: &types::Exp) -> quote::__rt::TokenStream {
    match e {
        Exp::Identifier { name } => {
            // NOTE(arjun): Writing quote! { #x } would treat x as a string and
            // generate the Rust code "x". This code is based on the following
            // example:
            // https://docs.rs/quote/0.6.12/quote/macro.quote.html#constructing-identifiers
            let q_id = syn::Ident::new(name, Span::call_site());
            quote! { #q_id }
        },
        Exp::Number { value } => quote! { #value },
        Exp::String { value } => quote! { #value },
        Exp::BinOp { op, e1, e2 } => {
            let q_op = compile_op(op);
            let q_e1 = compile(&e1);
            let q_e2 = compile(&e2);
            quote! { #q_e1 #q_op #q_e2 }
        },
        _ => panic!("wtf")
    }
}

fn main() {
    use std::io::Read;
    let args: Vec<_> = std::env::args().collect();
    let mut file = std::fs::File::open(&args[1]).expect("file not found");
    let mut json_string = String::new();
    file.read_to_string(&mut json_string).expect("file read failed");
    let exp: Exp = serde_json::from_str(&json_string).expect("failed to parse JSON");
    let s = compile(&exp);
    println!("{}", s);
}
