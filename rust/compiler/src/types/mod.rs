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
use std::collections::HashMap;

use im_rc::{
    HashMap as ImHashMap,
    HashSet as ImHashSet
};

use std::fmt;
use serde::Deserialize;

use std::fs::File;
use std::io::prelude::*;
use std::process::Stdio;
use std::process::Command;
use std::fs;

pub fn to_exp(filename: &str, code: &str, requests: &str) -> Exp {
    let f = File::create(filename).expect("creating file");
    let mut js_transform = Command::new("node")
        .arg("../../javascript/js-transform")
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
    return serde_json::from_str::<Exp>(&stdout)
        .unwrap_or_else(|exp| panic!("\n{:?} \nin \n{}", exp, &stdout));
}

#[derive(PartialEq, Debug, Clone)]
pub struct Arg { pub name: String, pub typ: Option<Typ> }

// https://users.rust-lang.org/t/need-help-with-serde-deserialize-with/18374
// https://stackoverflow.com/questions/41151080/deserialize-a-json-string-or-array-of-strings-into-a-vec/43627388#43627388
impl<'de> ::serde::Deserialize<'de> for Arg {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error> where D: ::serde::Deserializer<'de> {
        struct Visitor;

        impl<'de> ::serde::de::Visitor<'de> for Visitor {
            type Value = Arg;

            fn expecting(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                write!(f, "a string")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E> where E: ::serde::de::Error {
                Ok(Arg { name: v.to_string(), typ: None })
            }
        }

        deserializer.deserialize_any(Visitor)
    }
}

/// Deserializes a string or a sequence of strings into a vector of the target type.
pub fn deserialize_args<'de, T, D>(deserializer: D) -> Result<Vec<T>, D::Error>
    where T: ::serde::Deserialize<'de>, D: ::serde::Deserializer<'de> {

    struct Visitor<T>(::std::marker::PhantomData<T>);

    impl<'de, T> ::serde::de::Visitor<'de> for Visitor<T>
        where T: ::serde::Deserialize<'de> {

        type Value = Vec<T>;

        fn expecting(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
            write!(f, "a string or sequence of strings")
        }

        fn visit_seq<A>(self, visitor: A) -> Result<Self::Value, A::Error>
            where A: ::serde::de::SeqAccess<'de> {

            ::serde::Deserialize::deserialize(::serde::de::value::SeqAccessDeserializer::new(visitor))
        }
    }

    deserializer.deserialize_any(Visitor(::std::marker::PhantomData))
}

/*

    What I really want to do is to use Union(HashSet<Typ>)
    and to derive Hash, so that Typ's could be used as hash values.
    HashSet itself can't be hashed though because its elements are ordered.
    It might be possible to use + or XOR to create our own HashSet hashing,
    but that would be slightly unsafe.

    The other set implementation we could use is BTreeSet.

*/
#[derive(PartialEq, Eq, Debug, Deserialize, Clone, Hash)]
pub enum Typ {
    I32,
    F64,
    Bool,
    String,
    Unknown,
    Undefined,
    Metavar(usize),
    Ref(Box<Typ>),
    #[serde(skip)]
    Union(ImHashSet<Typ>),
    #[serde(skip)]
    Object(ImHashMap<String, Typ>),
    ResponseCallback,
    RustType(usize)
}

impl Typ {

    pub fn has_metavars(&self) -> bool {
        match self {
            Typ::Metavar(_) => true,
            Typ::Ref(t) => t.has_metavars(),
            Typ::Union(hs) => hs.iter().fold(false, |b, t| b || t.has_metavars()),
            Typ::I32 => false,
            Typ::F64 => false,
            Typ::Bool => false,
            Typ::String => false,
            Typ::Unknown => false,
            Typ::Undefined => false,
            Typ::Object(ts) => ts.iter()
                .fold(false, |b, (_, t)| b || t.has_metavars()),
            Typ::ResponseCallback => false,
            _ => unimplemented!()
        }
    }

    pub fn occurs_in(&self, x: usize) -> bool {
        match self {
            Typ::Metavar(y) => x == *y,
            Typ::Ref(t) => t.occurs_in(x),
            Typ::Union(hs) => hs.iter().fold(false, |b, t| b || t.occurs_in(x)),
            Typ::I32 => false,
            Typ::F64 => false,
            Typ::Bool => false,
            Typ::String => false,
            Typ::Unknown => false,
            Typ::Undefined => false,
            _ => unimplemented!()
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
            Typ::Union(hs) => {
                for t in hs.iter_mut() {
                    t.apply_subst(subst);
                }
            },
            Typ::I32 => (),
            Typ::F64 => (),
            Typ::Bool => (),
            Typ::String => (),
            Typ::Unknown => (),
            Typ::Undefined => (),
            Typ::Object(ts) => {
                for t in ts.iter_mut() {
                    t.apply_subst(subst)
                }
            },
            Typ::ResponseCallback => (),
            _ => unimplemented!()
        }
    }

    pub fn fake_hash_string(&self) -> String {
        match self {
            Typ::Metavar(y) => return "a".to_string() + (*y).to_string().as_str(),
            Typ::Ref(t) => return "b".to_string() + t.fake_hash_string().as_str(),
            Typ::Union(hs) => {
                let mut ret = "c".to_string();
                let mut hs2: Vec<String> = hs.iter().map(|t| t.fake_hash_string()).collect();
                hs2.sort();
                for t in hs2.iter() {
                    ret += t;
                }
                return ret;
            }
            Typ::I32 => return "d".to_string(),
            Typ::F64 => return "e".to_string(),
            Typ::Bool => return "f".to_string(),
            Typ::String => return "g".to_string(),
            Typ::Unknown => return "h".to_string(),
            Typ::Undefined => return "i".to_string(),
            Typ::Object(ts) => {
                let mut ret = "j".to_string();
                let mut ts2: Vec<String> = ts.iter().map(|(k,v)| k.to_string() + v.fake_hash_string().as_str()).collect();
                ts2.sort();
                for t in ts2.iter() {
                    ret += t;
                }
                return ret;
            }
            Typ::ResponseCallback => return "k".to_string(),
            _ => unimplemented!()
        }
    }
}

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
#[serde(tag = "kind", rename_all = "camelCase")]
pub enum Exp {
    Unknown { },
    Integer { value: i32 },
    Number { value: f64 },
    #[serde(rename = "boolean")]
    Bool { value: bool },
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
        #[serde(rename = "callbackArgs", deserialize_with = "deserialize_args")] callback_args: Vec<Arg>,
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
    From { exp: Box<Exp>, field: String }
}

// https://stackoverflow.com/a/42661287
fn vec_to_string(v: &[Exp]) -> String {
    return v.iter().fold(String::new(), |acc, num| acc + &num.to_string() + ",\n");
}

impl fmt::Display for Exp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
       match self {
            Exp::Unknown { } => write!(f, "Unknown"),
            Exp::Integer { value } => write!(f, "Integer({})", value),
            Exp::Number { value } => write!(f, "Number({})", value),
            Exp::Bool { value } => write!(f, "Bool({})", value),
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
            Exp::Callback { event, event_arg, callback_args, callback_clos, body } => write!(f, "Callback({}, {}, {:?}, {}, {})", event, event_arg, callback_args, callback_clos, vec_to_string(body)),
            Exp::Loopback { event, event_arg, callback_clos, id } => write!(f, "Loopback({}, {}, {}, {})", event, event_arg, callback_clos, id),
            Exp::Label { name, body } => write!(f, "Label({}, {})", name, vec_to_string(body)),
            Exp::Break { name, value } => write!(f, "Break({}, {})", name, value),
            Exp::Clos { tenv } => write!(f, "Clos({:?}", tenv),
            Exp::Array { exps } => write!(f, "Array({})", vec_to_string(exps)),
            Exp::Index { e1, e2 } => write!(f, "Index({}, {})", e1, e2),
            Exp::Ref { e } => write!(f, "Ref({})", e),
            Exp::Deref { e } => write!(f, "Deref({})", e),
            Exp::SetRef { e1, e2 } => write!(f, "SetRef({}, {})", e1, e2),
            Exp::PrimApp { event, event_args } => write!(f, "PrimApp({}, {})", event, vec_to_string(event_args))
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
    use crate::types::{Exp, Op2, LVal, Arg};

    use std::collections::HashMap;

    use im_rc::{
        HashSet as ImHashSet
    };

    pub fn t_union(t1: Typ, t2: Typ) -> Typ {
        match t1 {
            Typ::Union(ts) => {
                let mut ts2 = ts.clone();
                ts2.insert(t2);
                return Typ::Union(ts2);
            },
            _ => {
                let mut ts2 = ImHashSet::new();
                ts2.insert(t1);
                ts2.insert(t2);
                return Typ::Union(ts2);
            }
        }
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

    pub fn bool_(value: bool) -> Exp {
        return Bool { value };
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

    pub fn let_(name: &str, typ: Option<Typ>, named: Exp) -> Exp {
        return Let { name: name.to_string(), typ: typ, named: Box::new(named) };
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

    pub fn callback(event: &str, event_arg: Exp, callback_args: Vec<Arg>, callback_clos: Exp, body: Vec<Exp>) -> Exp {
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