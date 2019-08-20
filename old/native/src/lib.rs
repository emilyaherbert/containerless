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

// Deleted stuff is now in main code.

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