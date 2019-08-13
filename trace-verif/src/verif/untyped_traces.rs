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

use std::fmt;

use crate::verif::{
    transformer::Transformer,
    assertions::Assertions
};

#[derive(PartialEq, Debug, Deserialize, Clone)]
pub enum Op2 {
    #[serde(rename = "+")]
    Add,
    #[serde(rename = "-")]
    Sub,
    #[serde(rename = ">")]
    GT,
    #[serde(rename = "===")]
    StrictEq
}

#[derive(PartialEq, Debug, Deserialize, Clone)]
pub enum Typ {
    I32,
    F64,
    Bool,
    String,
    Unknown,
    Undefined,
    Metavar(usize),
    Ref(Box<Typ>),
    Union(Box<Typ>, Box<Typ>),
    Object(Vec<(String, Typ)>)
}

impl Typ {

    pub fn has_metavars(&self) -> bool {
        match self {
            Typ::Metavar(_) => true,
            Typ::Ref(t) => t.has_metavars(),
            Typ::Union(t1, t2) => t1.has_metavars() || t2.has_metavars(),
            Typ::I32 => false,
            Typ::F64 => false,
            Typ::Bool => false,
            Typ::String => false,
            Typ::Unknown => false,
            Typ::Undefined => false,
            Typ::Object(ts) => ts.iter()
                .fold(false, |b, (_, t)| b || t.has_metavars())
        }
    }

    pub fn occurs_in(&self, x: usize) -> bool {
        match self {
            Typ::Metavar(y) => x == *y,
            Typ::Ref(t) => t.occurs_in(x),
            Typ::Union(t1, t2) => t1.occurs_in(x) || t2.occurs_in(x),
            Typ::I32 => false,
            Typ::F64 => false,
            Typ::Bool => false,
            Typ::String => false,
            Typ::Unknown => false,
            Typ::Undefined => false,
            Typ::Object(ts) => unimplemented!()
        }
    }

    pub fn apply_subst(&mut self,
        subst: &std::collections::HashMap<usize, Typ>) -> () {
        match self {
            Typ::Metavar(x) => match subst.get(x) {
                None => (),
                Some(t) => *self = t.clone()
            },
            Typ::Ref(t) => t.apply_subst(subst),
            Typ::Union(t1, t2) => {
                t1.apply_subst(subst);
                t2.apply_subst(subst)
            },
            Typ::I32 => (),
            Typ::F64 => (),
            Typ::Bool => (),
            Typ::String => (),
            Typ::Unknown => (),
            Typ::Undefined => (),
            Typ::Object(ts) => {
                for (_, t) in ts.iter_mut() {
                    t.apply_subst(subst)
                }
            }
        }
    }


}

#[derive(PartialEq, Debug, Deserialize, Clone)]
#[serde(tag = "kind", rename_all = "camelCase")]
pub enum Exp {
    Unknown { },
    Integer { value: i32 },
    Number { value: f64 },
    Identifier { name: String },
    From { exp: Box<Exp>, field: String },
    #[serde(rename = "string")]
    Stringg { value: String },
    Undefined {},
    #[serde(rename = "binop")]
    BinOp { op: Op2, e1: Box<Exp>, e2: Box<Exp> },
    If {
        cond: Box<Exp>,
        #[serde(rename = "truePart")] true_part: Vec<Exp>,
        #[serde(rename = "falsePart")] false_part: Vec<Exp>
    },
    While { cond: Box<Exp>, body: Box<Exp> },
    Let {
        name: String,
        #[serde(default)]
        typ: Option<Typ>,
        named: Box<Exp>
    },
    Set { name: LVal, named: Box<Exp> },
    Block { body: Vec<Exp> },
    Callback {
        event: String,
        #[serde(rename = "eventArg")] event_arg: Box<Exp>,
        #[serde(rename = "callbackArgs")] callback_args: Vec<String>,
        #[serde(rename = "clos")] callback_clos: Box<Exp>,
        body: Vec<Exp>
    },
    #[serde(skip)]
    Loopback {
        event: String,
        event_arg: Box<Exp>,
        callback_clos: Box<Exp>,
        id: i32
    },
    Label { name: String, body: Vec<Exp> },
    Break { name: String, value: Box<Exp> },
    Clos { tenv: HashMap<String,Exp> },
    Array { exps: Vec<Exp> },
    Index { e1: Box<Exp>, e2: Box<Exp> },
    #[serde(skip)]
    Ref { e: Box<Exp> },
    #[serde(skip)]
    Deref { e: Box<Exp> },
    #[serde(skip)]
    SetRef { e1: Box<Exp>, e2: Box<Exp> },
    PrimApp {
        event: String,
        #[serde(rename = "eventArgs")] event_args: Vec<Exp>
    }
}

#[derive(PartialEq, Debug, Deserialize, Clone)]
#[serde(tag = "kind", rename_all = "camelCase")]
pub enum LVal {
    Identifier { name: String },
    From { exp: Box<Exp>, field: String },
    //Tuple { elems: Vec<String> }
}

// https://stackoverflow.com/a/42661287
fn vec_to_string(v: &[Exp]) -> String {
    return v.iter().fold(String::new(), |acc, num| acc + &num.to_string() + ",\n");
}

fn vec_to_string2(v: &[String]) -> String {
    return v.iter().fold(String::new(), |acc, num| acc + &num.to_string() + ",\n");
}

impl fmt::Display for Exp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
       match self {
            Exp::Unknown { } => write!(f, "Unknown"),
            Exp::Integer { value } => write!(f, "Integer({})", value),
            Exp::Number { value } => write!(f, "Number({})", value),
            Exp::Identifier { name } => write!(f, "Identifier({})", name),
            Exp::From { exp, field } => write!(f, "From({},{})", exp, field),
            Exp::Stringg { value } => write!(f, "Stringg({})", value),
            Exp::Undefined { } => write!(f, "Undefined"),
            Exp::BinOp { op, e1, e2 } => write!(f, "BinOp({:?}, {}, {})", op, e1, e2),
            Exp::If { cond, true_part, false_part } => write!(f, "If({} \n[{}]\n else \n[{}]\n)", cond, vec_to_string(true_part), vec_to_string(false_part)),
            Exp::While { cond, body } => write!(f, "While({}, {})", cond, body),
            Exp::Let { name, typ, named } => write!(f, "Let({} : {:?}, {})", name, typ, named),
            Exp::Set { name, named } => write!(f, "Set({:?}, {})", name, named),
            Exp::Block { body } => write!(f, "Block(\n[{}])", vec_to_string(body)),
            Exp::Callback { event, event_arg, callback_args, callback_clos, body } => write!(f, "Callback({}, {}, {}, {}, {})", event, event_arg, vec_to_string2(callback_args), callback_clos, vec_to_string(body)),
            Exp::Loopback { event, event_arg, callback_clos, id } => write!(f, "Loopback({}, {}, {}, {})", event, event_arg, callback_clos, id),
            Exp::Label { name, body } => write!(f, "Label({}, {})", name, vec_to_string(body)),
            Exp::Break { name, value } => write!(f, "Break({}, {})", name, value),
            Exp::Clos { tenv } => write!(f, "Clos({:?}", tenv),
            Exp::Array { exps } => write!(f, "Array({})", vec_to_string(exps)),
            Exp::Index { e1, e2 } => write!(f, "Index({}, {})", e1, e2),
            Exp::Ref { e } => write!(f, "Ref({})", e),
            Exp::Deref { e } => write!(f, "Deref({})", e),
            Exp::SetRef { e1, e2 } => write!(f, "SetRef({}, {})", e1, e2),
            Exp::PrimApp { event, event_args } => write!(f, "PrimApp({}, {})", event, vec_to_string(event_args)),
            _ => write!(f, "TODO")
       }
    }
}

pub mod constructors {

    // NOTE(arjun): I wish we could use this macro to avoid writing all these
    // constructors:
    //
    // https://docs.rs/derive-new/0.5.7/derive_new/
    //
    // But, I really don't want the new prefix on all names, and I want the
    // constructors to take care of allocating strings and boxes.

    use super::Exp::*;
    use super::Typ;
    use crate::verif::untyped_traces::{Exp, Op2, LVal};

    use std::collections::HashMap;

    pub fn t_union(t1: Typ, t2: Typ) -> Typ {
        Typ::Union(Box::new(t1), Box::new(t2))
    }

    pub fn t_ref(t: Typ) -> Typ {
        Typ::Ref(Box::new(t))
    }

    pub fn unknown() -> Exp {
        return Unknown { };
    }

    pub fn integer(value: i32) -> Exp {
        return Integer { value: value };
    }

    pub fn number(value: f64) -> Exp {
        return Number { value };
    }

    pub fn id(name: &str) -> Exp {
        return Identifier { name: name.to_string() };
    }

    pub fn from(exp: Exp, field: &str) -> Exp {
        return From { exp: Box::new(exp), field: field.to_string() };
    }

    pub fn string(value: &str) -> Exp {
        return Stringg { value: value.to_string() };
    }

    pub fn undefined() -> Exp {
        return Undefined { };
    }

    pub fn binop(op: &Op2, e1: Exp, e2: Exp) -> Exp {
        return BinOp { op: op.clone(), e1: Box::new(e1), e2: Box::new(e2) }
    }

    pub fn if_(cond: Exp, true_part: Vec<Exp>, false_part: Vec<Exp>) -> Exp {
        return If { cond: Box::new(cond), true_part, false_part };
    }

    pub fn while_(cond: Exp, body: Exp) -> Exp {
        return While { cond: Box::new(cond), body: Box::new(body) };
    }

    pub fn let_(name: &str, named: Exp) -> Exp {
        return Let { name: name.to_string(), typ: None, named: Box::new(named) };
    }

    pub fn set(name: LVal, named: Exp) -> Exp {
        return Set { name: name, named: Box::new(named) };
    }

    pub fn ref_(e: Exp) -> Exp {
        Ref { e: Box::new(e) }
    }

    pub fn deref(e: Exp) -> Exp {
        Deref { e: Box::new(e) }
    }

    pub fn setref(e1: Exp, e2: Exp) -> Exp {
        SetRef { e1: Box::new(e1), e2: Box::new(e2) }
    }

    // NOTE(emily): Apparently its bad Rust to receive a Vec<T>
    // NOTE(arjun): What is the alternative?
    pub fn block(body: Vec<Exp>) -> Exp {
        return Block { body: body };
    }

    pub fn callback(event: &str, event_arg: Exp, callback_args: Vec<String>, callback_clos: Exp, body: Vec<Exp>) -> Exp {
        return Callback {
            event: event.to_string(),
            event_arg: Box::new(event_arg),
            callback_args: callback_args,
            callback_clos: Box::new(callback_clos),
            body: body
        };
    }

    pub fn loopback(event: &str, event_arg: Exp, callback_clos: Exp, id: i32) -> Exp {
        return Loopback { event: event.to_string(),
            callback_clos: Box::new(callback_clos),
            event_arg: Box::new(event_arg), id: id };
    }

    pub fn label(name: &str, body: Vec<Exp>) -> Exp {
        return Label { name: name.to_string(), body: body };
    }

    pub fn break_(name: &str, value: Exp) -> Exp {
        return Break { name: name.to_string(), value: Box::new(value) };
    }

    pub fn array(exps: Vec<Exp>) -> Exp {
        return Array { exps: exps };
    }

    pub fn index(e1: Exp, e2: Exp) -> Exp {
        return Index { e1: Box::new(e1), e2: Box::new(e2) };
    }

    pub fn prim_app(event: &str, event_args: Vec<Exp>) -> Exp {
        return PrimApp { event: event.to_string(), event_args: event_args };
    }

    pub fn clos(tenv: HashMap<String, Exp>) -> Exp {
        return Clos { tenv: tenv };
    }

}

#[cfg(test)]
mod tests {
    use super::*;
    use super::constructors::*;
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
        js_transform.stdin.as_mut()
            .expect("opening stdin")
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

        decontainerized_js.stdin.as_mut()
            .expect("opening stdin")
            .write_all(requests.as_bytes())
            .expect("failed to write requests");

        let exit = decontainerized_js.wait()
            .expect("running decontainerized function");
        assert!(exit.success(), "non-zero exit code from function");

        let mut stdout = String::new();
        decontainerized_js.stdout.unwrap().read_to_string(&mut stdout)
            .expect("reading stdout");
        // TODO(arjun): We should only remove the file after the calling test
        // case succeeds. How can we do this neatly? Destructors?
        fs::remove_file(filename).expect("removing file");
        let exp = serde_json::from_str::<Exp>(&stdout);

        if exp.is_err() {
            panic!("\n{:?} \nin \n{}", exp, &stdout);
        }

        let exp2 = exp.unwrap();

        let mut assertions = Assertions::new();
        assertions.assert_supposed_grammar(&exp2);
        assertions.assert_unique_names(&exp2);

        //println!("{:?}\n", exp);

        // let mut transformer = Transformer::new();
        // let mut lifted = transformer.transform(&(exp.unwrap()));
        let mut e = exp.unwrap();
        crate::verif::typeinf::typeinf(&mut e).unwrap();
        return e;
    }

    #[test]
    pub fn try_test() {
        let _handle = test_harness("try_test.js", r#"
            let containerless = require("../containerless");
            containerless.listen(function(req, resp) {
                resp('Hello, world!');
            });
        "#, "");
    }

    // #[test]
    pub fn try_test2() {
        let handle = test_harness("try_test2.js", r#"
            let containerless = require("../containerless");
            let str = 'Got a response!';
            containerless.listen(function(req, resp) {
                // console.log(str);
                console.error(str);
                resp(req);
            });
        "#, "request1
        request2");
    }

    // #[test]
    pub fn trace_with_unknown() {
        let handle = test_harness("trace_with_unknown.js", r#"
            let containerless = require("../containerless");
            containerless.listen(function(req, resp) {
                if (req === 'hello') {
                    resp('goodbye');
                }
                else {
                    resp('bad');
                }
            });
        "#, "hello");
        // assert!(false);
    }

    // #[test]
    pub fn multiple_callbacks() {
        let handle = test_harness("multiple_callbacks.js", r#"
            let containerless = require('../containerless');

            let foo = 42;

            containerless.listen(function(req, resp) {
                console.error('Got a response');
                let bar = foo + 1;
                resp(req);

                containerless.get('http://people.cs.umass.edu/~emilyherbert/', function(response) {
                    console.error(response);
                    let baz = foo + bar;
                });

                console.error('All done!');
            });
        "#, "hello
        goodbye
        hello again");

        let mut tenv1 = HashMap::new();
        tenv1.insert("foo00".to_string(), id("foo00"));

        let mut tenv2 = HashMap::new();
        tenv2.insert("bar00".to_string(), id("bar00"));
        tenv2.insert("foo00".to_string(), from(deref(id("$clos")), "foo00"));

        let target = if_(
                binop(&Op2::StrictEq, id("$CBID"), integer(1)),
                vec![
                    let_("$clos", index(id("$CBARGS"), integer(0))),
                    let_("$request", index(id("$CBARGS"), integer(1))),
                    let_("$responseCallback", index(id("$CBARGS"), integer(2))),
                    label("$return", vec![
                        let_("req", ref_(deref(id("$request")))),
                        let_("resp", ref_(deref(id("$responseCallback")))),
                        prim_app("console.log", vec![from(deref(id("console")), "error"), string("Got a response")]), // console.error is for testing
                        let_("bar00", ref_(binop(&Op2::Add, from(deref(id("$clos")), "foo00"), number(1.0)))),
                        let_("app200", ref_(block(vec![
                            label("$return", vec![
                                let_("response", ref_(deref(id("req")))),
                                prim_app("send", vec![from(deref(id("resp")), "send"), deref(id("response"))])
                            ])
                        ]))),
                        let_("fun100", ref_(clos(tenv2))),
                        let_("app300", ref_(block(vec![
                            loopback("get", string("http://people.cs.umass.edu/~emilyherbert/"), deref(id("fun100")), 2)
                        ]))),
                        prim_app("console.log", vec![from(deref(id("console")), "error"), string("All done!")]) // console.error is for testing
                    ])
                ],
                vec![
                    if_(
                        binop(&Op2::StrictEq, id("$CBID"), integer(2)),
                        vec![
                            let_("$clos", index(id("$CBARGS"), integer(0))),
                            let_("$response", index(id("$CBARGS"), integer(1))),
                            label("$return", vec![
                                let_("response", ref_(deref(id("$response")))),
                                prim_app("console.log", vec![from(deref(id("console")), "error"), deref(id("response"))]),
                                let_("baz00", ref_(binop(&Op2::Add, from(deref(id("$clos")), "foo00"), from(deref(id("$clos")), "bar00"))))
                            ])
                        ],
                        vec![
                            block(vec![
                                let_("foo00", ref_(number(42.0))),
                                let_("fun000", ref_(clos(tenv1))),
                                let_("app000", ref_(block(vec![
                                    loopback("listen", number(0.0), deref(id("fun000")), 1),
                                    unknown()
                                ]))),
                                unknown()
                            ])
                        ]
                    )
                ]
            );

        //println!("{}\n", handle);
        //println!("{}", target);

        assert!(handle == target);

        /*

        {
        "kind": "block",
        "body": [
            {
            "kind": "let",
            "name": "foo00",
            "named": {
                "kind": "number",
                "value": 42
            }
            },
            {
            "kind": "let",
            "name": "fun000",
            "named": {
                "kind": "clos",
                "tenv": {
                "foo00": {
                    "kind": "identifier",
                    "name": "foo00"
                }
                }
            }
            },
            {
            "kind": "let",
            "name": "app000",
            "named": {
                "kind": "block",
                "body": [
                {
                    "kind": "callback",
                    "event": "listen",
                    "eventArg": {
                    "kind": "number",
                    "value": 0
                    },
                    "callbackArgs": [
                    "$clos",
                    "$request",
                    "$responseCallback"
                    ],
                    "clos": {
                    "kind": "identifier",
                    "name": "fun000"
                    },
                    "body": [
                    {
                        "kind": "label",
                        "name": "$return",
                        "body": [
                        {
                            "kind": "let",
                            "name": "req",
                            "named": {
                            "kind": "identifier",
                            "name": "$request"
                            }
                        },
                        {
                            "kind": "let",
                            "name": "resp",
                            "named": {
                            "kind": "identifier",
                            "name": "$responseCallback"
                            }
                        },
                        {
                            "kind": "primApp",
                            "event": "console.log",
                            "eventArgs": [
                            {
                                "kind": "from",
                                "exp": {
                                "kind": "identifier",
                                "name": "console"
                                },
                                "field": "error"
                            },
                            {
                                "kind": "string",
                                "value": "Got a response"
                            }
                            ]
                        },
                        {
                            "kind": "let",
                            "name": "bar00",
                            "named": {
                            "kind": "binop",
                            "op": "+",
                            "e1": {
                                "kind": "from",
                                "exp": {
                                "kind": "identifier",
                                "name": "$clos"
                                },
                                "field": "foo00"
                            },
                            "e2": {
                                "kind": "number",
                                "value": 1
                            }
                            }
                        },
                        {
                            "kind": "let",
                            "name": "app200",
                            "named": {
                            "kind": "block",
                            "body": [
                                {
                                "kind": "label",
                                "name": "$return",
                                "body": [
                                    {
                                    "kind": "let",
                                    "name": "response",
                                    "named": {
                                        "kind": "identifier",
                                        "name": "req"
                                    }
                                    },
                                    {
                                    "kind": "primApp",
                                    "event": "send",
                                    "eventArgs": [
                                        {
                                        "kind": "from",
                                        "exp": {
                                            "kind": "identifier",
                                            "name": "resp"
                                        },
                                        "field": "send"
                                        },
                                        {
                                        "kind": "identifier",
                                        "name": "response"
                                        }
                                    ]
                                    }
                                ]
                                }
                            ]
                            }
                        },
                        {
                            "kind": "let",
                            "name": "fun100",
                            "named": {
                            "kind": "clos",
                            "tenv": {
                                "foo00": {
                                "kind": "from",
                                "exp": {
                                    "kind": "identifier",
                                    "name": "$clos"
                                },
                                "field": "foo00"
                                },
                                "bar00": {
                                "kind": "identifier",
                                "name": "bar00"
                                }
                            }
                            }
                        },
                        {
                            "kind": "let",
                            "name": "app300",
                            "named": {
                            "kind": "block",
                            "body": [
                                {
                                "kind": "callback",
                                "event": "get",
                                "eventArg": {
                                    "kind": "string",
                                    "value": "http:\/\/people.cs.umass.edu\/~emilyherbert\/"
                                },
                                "callbackArgs": [
                                    "$clos",
                                    "$response"
                                ],
                                "clos": {
                                    "kind": "identifier",
                                    "name": "fun100"
                                },
                                "body": [
                                    {
                                    "kind": "label",
                                    "name": "$return",
                                    "body": [
                                        {
                                        "kind": "let",
                                        "name": "response",
                                        "named": {
                                            "kind": "identifier",
                                            "name": "$reponse"
                                        }
                                        },
                                        {
                                        "kind": "primApp",
                                        "event": "console.log",
                                        "eventArgs": [
                                            {
                                            "kind": "from",
                                            "exp": {
                                                "kind": "identifier",
                                                "name": "console"
                                            },
                                            "field": "error"
                                            },
                                            {
                                            "kind": "identifier",
                                            "name": "response"
                                            }
                                        ]
                                        },
                                        {
                                        "kind": "let",
                                        "name": "baz00",
                                        "named": {
                                            "kind": "binop",
                                            "op": "+",
                                            "e1": {
                                            "kind": "from",
                                            "exp": {
                                                "kind": "identifier",
                                                "name": "$clos"
                                            },
                                            "field": "foo00"
                                            },
                                            "e2": {
                                            "kind": "from",
                                            "exp": {
                                                "kind": "identifier",
                                                "name": "$clos"
                                            },
                                            "field": "bar00"
                                            }
                                        }
                                        }
                                    ]
                                    }
                                ]
                                }
                            ]
                            }
                        },
                        {
                            "kind": "primApp",
                            "event": "console.log",
                            "eventArgs": [
                            {
                                "kind": "from",
                                "exp": {
                                "kind": "identifier",
                                "name": "console"
                                },
                                "field": "error"
                            },
                            {
                                "kind": "string",
                                "value": "All done!"
                            }
                            ]
                        }
                        ]
                    }
                    ]
                }
                ]
            }
            }
        ]
        }

        */
    }

}
