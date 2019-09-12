#[macro_use]
extern crate duct;

use clap::{App, Arg, SubCommand};

mod codegen;
mod gen;
mod types;
mod verif;
mod trace_js;

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
                .arg(Arg::with_name("input").short("i").help("Path to input").takes_value(true).required(true)),
        )
        .get_matches();

    if let Some(m) = matches.subcommand_matches("test-tracing") {
        let file = m.value_of("file").unwrap();
        let input = m.value_of("input").unwrap();
        let trace_json = trace_js::trace_with_files(file, input);
        println!("{}", trace_json);
    }
}

#[cfg(test)]
mod tests {

    use crate::{
        gen::gen,
        types::{to_exp, Exp},
        verif::verify,
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
