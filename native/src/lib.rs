#![macro_use]
extern crate quote;
extern crate syn;
extern crate serde;
extern crate serde_json;
extern crate libloading;
#[macro_use]
extern crate duct;
#[macro_use]
extern crate neon;
mod types;

use types::*;
use quote::*;
use quote::__rt::TokenStream;
use proc_macro2::Span;
use neon::prelude::*;

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

fn compile_program(stmt: &Stmt) -> quote::__rt::TokenStream {
    let q_stmt = compile_stmt(stmt);
    quote! {
        #[no_mangle]
        pub extern "C" fn trace_main() -> f64 {
            #q_stmt
        }
    }
}

fn compile(mut cx: FunctionContext) -> JsResult<JsString> {
  use std::io::Write;

  let program = cx.argument::<JsString>(0)?.value();
  let stmt: Stmt = serde_json::from_str(&program)
    .expect("Failed to parse JSON.");
  let rust_code = compile_program(&stmt);
  let mut rs_file = std::fs::File::create("trace.rs")
    .expect("Could not create .rs file.");
  rs_file.write_all(format!("{}", rust_code).as_bytes())
    .expect("Could not write to file.");
  cmd!("rustc", "--crate-type", "cdylib", "trace.rs").run()
    .expect("Compiling to Rust failed.");

  Ok(cx.string("Done compiling."))
}

fn run(mut cx: FunctionContext) -> JsResult<JsNumber> {

  let lib = libloading::Library::new("libtrace.so")
      .expect("Loading .so failed.");

  unsafe {
    let fun: libloading::Symbol<unsafe extern fn() -> f64> =
      lib.get(b"trace_main")
        .expect("Symbol trace_main not found.");
    return Ok(cx.number(fun()));
  }

  Ok(cx.number(1))
}

register_module!(mut m, {
    m.export_function("compile", compile)?;
    m.export_function("run", run)?;
    Ok(())
});