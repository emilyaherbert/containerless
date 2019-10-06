pub mod transformer;
pub mod typeinf;
//pub mod typeinf2;
//pub mod typeinf3;
pub mod assertions;
pub mod lift_callbacks;

use crate::{
    types::Exp,
    verif::{assertions::Assertions, lift_callbacks::LiftCallbacks, transformer::Transformer},
};

pub fn verify(exp: &Exp) -> Exp {
    let mut assertions = Assertions::new();
    let mut transformer = Transformer::new();
    let mut lift_callbacks = LiftCallbacks::new();

    assertions.assert_supposed_grammar(&exp);
    assertions.assert_unique_names(&exp);
    assertions.assert_all_options_are_none(&exp);

    let mut exp2 = transformer.transform(&exp);
    crate::verif::typeinf::typeinf(&mut exp2).unwrap();
    let exp3 = lift_callbacks.lift(&exp2);

    return exp3;
}

pub fn verify_from_file(filename: &str) -> Exp {
    let json = std::fs::read_to_string(filename)
        .expect(&format!("could not read file {}", filename));
    let exp = serde_json::from_str::<Exp>(&json)
        .unwrap_or_else(|exp| panic!("\n{:?} \nin \n{}", exp, &json));
    return verify(&exp);
}

#[cfg(test)]
mod tests {

    use crate::trace_js::to_exp;
    use crate::{
        types::Exp,
        verif::verify,
    };
    use super::super::codegen;
    use std::process::Command;
    use std::process::Stdio;
    use std::io::prelude::*;


    fn test_end_to_end(
        filename: &str,
        code: &str,
        js_requests: &str,
        rs_requests: &str) -> Vec<String> {
        let json = to_exp(filename, code, js_requests);
        std::fs::remove_file(filename).expect("removing file");
        let exp = serde_json::from_str::<Exp>(&json)
            .unwrap_or_else(|exp| panic!("\n{:?} \nin \n{}", exp, &json));
        let verified = verify(&exp);
        codegen::codegen(&verified, "../containerless-scaffold/src/containerless.rs");
        let mut decontainerized = Command::new("cargo")
            .args(["run", "ignored-arg", "--testing"].iter())
            .current_dir("../containerless-scaffold")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .expect("starting decontainerized function (in Rust)");
        decontainerized
            .stdin
            .as_mut()
            .expect("opening stdin")
            .write_all(rs_requests.as_bytes())
            .expect("failed to write requests");

        let exit = decontainerized
            .wait()
            .expect("running decontainerized function");
        assert!(exit.success(), "non-zero exit code from function");

        let mut stdout = String::new();
        decontainerized
            .stdout
            .unwrap()
            .read_to_string(&mut stdout)
            .expect("reading stdout");
            
        return stdout.split_terminator('\n').map(|s| s.to_string()).collect();
    }

    #[test]
    pub fn trivial_fixed_response() {
        let result = test_end_to_end(
            "try_test.js",
            r#"
            let containerless = require("../../javascript/containerless");
            containerless.listen(function(req) {
                containerless.respond("Hello, world!");
            });"#,
            "input",
            "input");
        assert_eq!(result, vec!["Hello, world!"]);
    }

    #[test]
    pub fn trivial_echo_path() {
        let result = test_end_to_end(
            "try_test.js",
            r#"
            let containerless = require("../../javascript/containerless");
            containerless.listen(function(req) {
                containerless.respond(req.path);
            });"#,
            "/apath",
            "/bpath");
        assert_eq!(result, ["/bpath"]);
    }

    #[test]
    pub fn trivial_conditional_with_unknown() {
        let result = test_end_to_end(
            "trivial_conditional_with_unknown.js",
            r#"
            let containerless = require("../../javascript/containerless");
            containerless.listen(function(req) {
                if (req.path === 'hello') {
                    containerless.respond("correct");
                }
                else {
                    containerless.respond("wrong");
                }
            });
        "#,
            "hello",
            "hello"
        );

        assert_eq!(result, ["correct"]);
    }
}
