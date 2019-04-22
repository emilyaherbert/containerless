#![macro_use]
#![allow(unreachable_code)]

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

fn compile_unary_op(op: &UnaryOp) -> TokenStream {
    match op {
        UnaryOp::Neg => quote! { - }
    }
}

fn compile_op(op: &BinOp) -> TokenStream {
    match op {
        BinOp::Add => quote! { + },
        BinOp::Sub => quote! { - },
        BinOp::Mul => quote! { * },
        BinOp::LT => quote! { < },
        BinOp::LTE => quote! { <= },
        BinOp::GT => quote! { > },
        BinOp::GTE => quote! { >= }
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
        Exp::Boolean { value } => quote! { #value },
        Exp::Input {} => quote! { trace_input },
        Exp::BinOp { op, e1, e2 } => {
            let q_op = compile_op(op);
            let q_e1 = compile_exp(&e1);
            let q_e2 = compile_exp(&e2);
            quote! { (#q_e1 #q_op #q_e2) }
        },
        Exp::UnaryOp { op, e } => {
          let q_op = compile_unary_op(op);
          let q_e = compile_exp(e);
          quote! { (#q_op #q_e) }
        }
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
        Stmt::If { test, then_part, else_part } => {
          let q_test = compile_exp(test);
          let q_then_part = compile_stmt(then_part);
          let q_else_part = compile_stmt(else_part);
          quote! {
            if #q_test { #q_then_part } else { #q_else_part }
          }
        }
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
        #[allow(unused_parens)]
        pub extern "C" fn trace_main(trace_input: f64) -> f64 {
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

  let trace_input = cx.argument::<JsNumber>(0)?.value();

  // NOTE(emily): cdylib produces a .dylib file on mac and a .so file on linux.
  // ¯\_(ツ)_/¯
  let lib = libloading::Library::new("libtrace.dylib")
      .or_else(|_| libloading::Library::new("libtrace.so"))
      .expect("Loading dynamic library failed.");

  unsafe {
    let fun: libloading::Symbol<unsafe extern fn(t_i: f64) -> f64> =
      lib.get(b"trace_main")
        .expect("Symbol trace_main not found.");
    return Ok(cx.number(fun(trace_input)));
  }

  Ok(cx.number(1))
}

register_module!(mut m, {
    m.export_function("compile", compile)?;
    m.export_function("run", run)?;
    Ok(())
});