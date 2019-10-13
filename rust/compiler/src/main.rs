#[macro_use]

extern crate duct;

use clap::{App, Arg, SubCommand};

mod codegen;
mod gen;
mod trace_js;
mod types;
mod verif;
mod tests;

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
    else {
        println!("Missing sub-command. Run --help for help.");
        std::process::exit(1);
    }
}
