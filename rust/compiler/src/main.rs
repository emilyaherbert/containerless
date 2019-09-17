#[macro_use]
extern crate duct;

use libloading::{Library, Symbol};
use clap::{App, Arg, SubCommand};

mod codegen;
mod gen;
mod trace_js;
mod types;
mod verif;
mod runtime;

fn main() {
    let matches = App::new("decontainerization")
        .subcommand(
            SubCommand::with_name("test-tracing")
                .about("Create a trace by running a JavaScript program on input")
                .arg(
                    Arg::with_name("file")
                        .short("f")
                        .help("Path to JavaScript file")
                        .takes_value(true)
                        .required(true),
                )
                .arg(
                    Arg::with_name("input")
                        .short("i")
                        .help("Path to input")
                        .takes_value(true)
                        .required(true),
                ),
        )
        .subcommand(
            SubCommand::with_name("test-codegen")
                .about("Verify and generate Rust code from a trace")
                .arg(
                    Arg::with_name("input")
                        .short("i")
                        .help("Path to trace (.json) file")
                        .takes_value(true)
                        .required(true),
                )
                .arg(
                    Arg::with_name("output")
                        .short("o")
                        .help("Path to output (.rs) file")
                        .takes_value(true)
                        .required(true),
                )
        )
        .subcommand(
            SubCommand::with_name("test-compiled-trace")
                .about("Execute a compiled trace on inputs")
                .arg(
                    Arg::with_name("file")
                        .short("f")
                        .help("Path to Rust library")
                        .takes_value(true)
                        .required(true),
                )
                .arg(
                    Arg::with_name("input")
                        .short("i")
                        .help("Path to input")
                        .takes_value(true)
                        .required(true),
                ),
        )
        .get_matches();

    if let Some(m) = matches.subcommand_matches("test-tracing") {
        let file = m.value_of("file").unwrap();
        let input = m.value_of("input").unwrap();
        let trace_json = trace_js::trace_with_files(file, input);
        println!("{}", trace_json);
    }
    else if let Some(m) = matches.subcommand_matches("test-codegen") {
        let input = m.value_of("input").unwrap();
        let output = m.value_of("output").unwrap();
        let exp = verif::verify_from_file(input);
        codegen::codegen(&exp, &output);
    }
    else if let Some(m) = matches.subcommand_matches("test-compiled-trace") {
        let file = m.value_of("file").unwrap();
        let input = m.value_of("input").unwrap();
        let lib = Library::new(file)
            .expect("failed to load DLL");

        let func: Symbol<fn() -> ()> = unsafe {
            lib.get(b"containerless")
                .expect("did not find containerless function")
        };
        func();
    }
    else {
        println!("Missing sub-command. Run --help for help.");
        std::process::exit(1);
    }
}

#[cfg(test)]
mod tests {

    use crate::trace_js::to_exp;
    use crate::{
        gen::gen,
        types::Exp,
        verif::verify,
    };

    fn test_harness(filename: &str, code: &str, requests: &str) -> Exp {
        // Turns a code string to Exp tree
        let exp = to_exp(filename, code, requests);
        // Checks assertions, calculates types
        let mut exp2 = verify(&serde_json::from_str(&exp).unwrap());
        // Translates typ's to RustType's, creates a usize -> Typ
        let rust_types = gen(&mut exp2);

        println!("{:?}", rust_types);
        //println!("{:#?}", exp2);

        return exp2;
    }

}
