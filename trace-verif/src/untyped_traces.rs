// These type definitions must be consistent with the definitions in TypeScript,
// which are found in /containerless/ts/exp.ts.
//
// We are using serde_json to automatically derive deserialization code. The
// following pages of documentation are relevant:
//
// - https://serde.rs/enum-representations.html
//
//   Illustrates the #[serde(tag = "kind")] directive.
//
// - https://serde.rs/attr-rename.html
//
//   Illustrates the #[serde(rename_all = "camelCase")] directive. Note that
//   the directive also renames type-tags. E.g., Exp::Undefined {} serializes
//   to { type: "undefined" } and not { type: "Undefined" }. The example does
//   not make this clear, but it is what we want.
//
// - I could not find documentation for the #[serde(rename = "+")] directive.
//   Instead, I guessed that it existed and it worked!
use serde::Deserialize;
use std::collections::HashMap;

#[derive(PartialEq, Debug, Deserialize)]
pub enum BinOp {
    #[serde(rename = "+")]
    Add,
    #[serde(rename = "-")]
    Sub,
    #[serde(rename = ">")]
    GT
}

#[derive(PartialEq, Debug, Deserialize)]
#[serde(tag = "kind", rename_all = "camelCase")]
pub enum Exp {
    Unknown { },
    Number { value: f64 },
    Identifier { name: String },
    From { exp: Box<Exp>, field: String },
    String { value: String },
    Undefined {},
    #[serde(rename = "binop")]
    BinOp { op: BinOp, e1: Box<Exp>, e2: Box<Exp> },
    If { cond: Box<Exp>, true_part: Box<Exp>, false_part: Box<Exp> },
    While { cond: Box<Exp>, body: Box<Exp> },
    Let { name: String, named: Box<Exp> },
    Set { name: LVal, named: Box<Exp> },
    Block { body: Vec<Exp> },
    Callback { event: String, eventArg: Box<Exp>, callbackArgs: Vec<String>,
        body: Vec<Exp> },
    Label { name: String, body: Vec<Exp> },
    Break { name: String, value: Box<Exp> },
    Clos { tenv: HashMap<String,Exp> },
    Array { exps: Vec<Exp> },
    PrimApp { event: String, eventArgs: Vec<Exp> }
}

#[derive(PartialEq, Debug, Deserialize)]
#[serde(tag = "kind", rename_all = "camelCase")]
pub enum LVal {
    Identifier { name: usize },
    From { exp: Box<Exp>, field: String }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::File;
    use std::io::prelude::*;
    use std::process::Stdio;
    use std::process::Command;
    use std::fs;

    fn test_harness(filename: &str, code: &str, requests: &str) -> Exp {
        let f = File::create(filename).expect("creating file");
        let mut js_transform = Command::new("node")
            .arg("../js-transform")
            .stdin(Stdio::piped())
            .stdout(f)
            .spawn()
            .expect("starting js-transform");
        js_transform.stdin.as_mut().expect("opening stdin")
            .write_all(code.as_bytes())
            .expect("failed to write to JS file");
        let exit = js_transform.wait().expect("running js-transform");
        assert!(exit.success());

        let mut decontainerized_js = Command::new("node")
            .arg(filename)
            .arg("test")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .expect("starting decontainerized function");
        decontainerized_js.stdin.as_mut().expect("opening stdin")
            .write_all(requests.as_bytes())
            .expect("failed to write requests");
        let exit = decontainerized_js.wait()
            .expect("running decontainerized function");
        assert!(exit.success());
        let mut stdout = String::new();
        decontainerized_js.stdout.unwrap().read_to_string(&mut stdout)
            .expect("reading stdout");
        fs::remove_file(filename).expect("removing file");
        let exp = serde_json::from_str::<Exp>(&stdout);
        if exp.is_err() {
            panic!("{:?} in {}", exp, &stdout);
        }
        return exp.unwrap();
    }


    #[test]
    pub fn try_test() {
        let handle = test_harness("try_test.js", r#"
            let containerless = require("../containerless");
            containerless.listen(function(req, resp) {
                resp('Hello, world!');
            });
        "#, "");
    }

    #[test]
    pub fn try_test2() {
        let handle = test_harness("try_test2.js", r#"
            let containerless = require("../containerless");
            containerless.listen(function(req, resp) {
                //console.log('Got a response');
                resp(req);
            });
        "#, "request1");
    }

}