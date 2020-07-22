#![allow(dead_code)]
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

use im_rc::{HashMap as ImHashMap, HashSet as ImHashSet};

use serde::Deserialize;
use std::fmt;

#[derive(PartialEq, Debug, Clone)]
pub struct Arg {
    pub name: String,
    pub typ: Option<Typ>,
}

// https://users.rust-lang.org/t/need-help-with-serde-deserialize-with/18374
// https://stackoverflow.com/questions/41151080/deserialize-a-json-string-or-array-of-strings-into-a-vec/43627388#43627388
impl<'de> ::serde::Deserialize<'de> for Arg {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: ::serde::Deserializer<'de>,
    {
        struct Visitor;

        impl<'de> ::serde::de::Visitor<'de> for Visitor {
            type Value = Arg;

            fn expecting(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                write!(f, "a string")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: ::serde::de::Error,
            {
                Ok(Arg {
                    name: v.to_string(),
                    typ: None,
                })
            }
        }

        deserializer.deserialize_any(Visitor)
    }
}

/// Deserializes a string or a sequence of strings into a vector of the target type.
pub fn deserialize_args<'de, T, D>(deserializer: D) -> Result<Vec<T>, D::Error>
where
    T: ::serde::Deserialize<'de>,
    D: ::serde::Deserializer<'de>,
{
    struct Visitor<T>(::std::marker::PhantomData<T>);

    impl<'de, T> ::serde::de::Visitor<'de> for Visitor<T>
    where
        T: ::serde::Deserialize<'de>,
    {
        type Value = Vec<T>;

        fn expecting(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
            write!(f, "a string or sequence of strings")
        }

        fn visit_seq<A>(self, visitor: A) -> Result<Self::Value, A::Error>
        where
            A: ::serde::de::SeqAccess<'de>,
        {
            ::serde::Deserialize::deserialize(::serde::de::value::SeqAccessDeserializer::new(
                visitor,
            ))
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
    Unionvar(usize),
    Ref(Box<Typ>),
    #[serde(skip)]
    Union(ImHashSet<Typ>),
    #[serde(skip)]
    Object(ImHashMap<String, Typ>),
    Array(Box<Typ>),
    ResponseCallback,
    RustType(usize),
}

impl Typ {
    pub fn has_vars(&self) -> bool {
        match self {
            Typ::Metavar(_) => true,
            Typ::Unionvar(_) => true,
            Typ::Ref(t) => t.has_vars(),
            Typ::Union(hs) => hs.iter().any(|t| t.has_vars()),
            Typ::I32 => false,
            Typ::F64 => false,
            Typ::Bool => false,
            Typ::String => false,
            Typ::Unknown => false,
            Typ::Undefined => false,
            Typ::Object(ts) => ts.iter().fold(false, |b, (_, t)| b || t.has_vars()),
            Typ::ResponseCallback => false,
            Typ::Array(t) => t.has_vars(),
            _ => unimplemented!(),
        }
    }

    pub fn occurs_in(&self, x: usize) -> bool {
        match self {
            Typ::Metavar(y) => x == *y,
            Typ::Ref(t) => t.occurs_in(x),
            Typ::Union(hs) => hs.iter().any(|t| t.occurs_in(x)),
            Typ::I32 => false,
            Typ::F64 => false,
            Typ::Bool => false,
            Typ::String => false,
            Typ::Unknown => false,
            Typ::Undefined => false,
            _ => unimplemented!(),
        }
    }

    pub fn apply_subst(&mut self, subst: &std::collections::HashMap<usize, Typ>) -> () {
        match self {
            Typ::Metavar(x) => match subst.get(x) {
                None => (),
                Some(t) => *self = t.flatten(),
            },
            Typ::Unionvar(_) => (),
            Typ::Ref(t) => t.apply_subst(subst),
            Typ::Union(hs) => {
                for t in hs.iter_mut() {
                    t.apply_subst(subst);
                }
            }
            Typ::I32 => (),
            Typ::F64 => (),
            Typ::Bool => (),
            Typ::String => (),
            Typ::Unknown => (),
            Typ::Undefined => (),
            Typ::Object(_) => (),
            Typ::ResponseCallback => (),
            Typ::Array(t) => t.apply_subst(subst),
            _ => unimplemented!(),
        }
    }

    pub fn apply_subst_strict(&mut self, subst: &std::collections::HashMap<usize, Typ>) -> () {
        match self {
            Typ::Metavar(x) => match subst.get(x) {
                None => panic!("Found free metavar."),
                Some(t) => *self = t.flatten(),
            },
            Typ::Unionvar(x) => match subst.get(x) {
                None => panic!("Did not find {} in subst.", x.to_string()),
                Some(t) => {
                    let new_t = t.flatten();
                    match new_t {
                        Typ::Union(typ_vec) => {
                            let mut typ_vec2 = typ_vec;
                            // Don't include my own name in my union type.
                            typ_vec2.remove(&Typ::Unionvar(*x));
                            *self = Typ::Union(typ_vec2);
                        }
                        new_t => {
                            *self = new_t;
                        }
                    }
                }
            },
            Typ::Ref(t) => t.apply_subst_strict(subst),
            Typ::Union(hs) => {
                for t in hs.iter_mut() {
                    t.apply_subst_strict(subst);
                }
                *self = self.flatten();
            }
            Typ::I32 => (),
            Typ::F64 => (),
            Typ::Bool => (),
            Typ::String => (),
            Typ::Unknown => (),
            Typ::Undefined => (),
            Typ::Object(ts) => {
                for t in ts.iter_mut() {
                    t.apply_subst_strict(subst)
                }
            }
            Typ::ResponseCallback => (),
            Typ::Array(t) => t.apply_subst_strict(subst),
            _ => unimplemented!(),
        }
    }

    fn flatten(&self) -> Typ {
        match self {
            Typ::Union(typ_vec) => {
                let mut new_typ_vec = ImHashSet::new();
                for t in typ_vec.iter() {
                    let t2 = t.flatten();
                    match t2 {
                        Typ::Union(typ_vec2) => {
                            new_typ_vec = new_typ_vec.union(typ_vec2);
                        }
                        t => {
                            new_typ_vec.insert(t);
                        }
                    }
                }
                if new_typ_vec.is_empty() {
                    panic!("Did not expect to find empty Union type.")
                } else if new_typ_vec.len() == 1 {
                    match new_typ_vec.iter().next() {
                        Some(new_typ) => return new_typ.to_owned(),
                        None => panic!("Something went wrong."),
                    }
                } else {
                    return Typ::Union(new_typ_vec);
                }
            }
            t => {
                return t.clone();
            }
        }
    }
}

#[derive(PartialEq, Debug, Deserialize, Clone)]
pub enum Op1 {
    #[serde(rename = "void")]
    Void,
    #[serde(rename = "typeof")]
    Typeof,
    #[serde(rename = "-")]
    Negative,
}

#[derive(PartialEq, Debug, Deserialize, Clone)]
pub enum Op2 {
    #[serde(rename = "+")]
    Add,
    #[serde(rename = "-")]
    Sub,
    #[serde(rename = "*")]
    Mul,
    #[serde(rename = "/")]
    Div,
    #[serde(rename = "===")]
    StrictEq,
    #[serde(rename = "!==")]
    StrictNotEq,
    #[serde(rename = ">")]
    GT,
    #[serde(rename = "<")]
    LT,
    #[serde(rename = ">=")]
    GTE,
    #[serde(rename = "<=")]
    LTE,
    #[serde(rename = "&&")]
    And,
    #[serde(rename = "||")]
    Or,
}

#[derive(PartialEq, Debug, Deserialize, Clone)]
#[serde(tag = "kind", rename_all = "camelCase")]
pub enum Exp {
    Unknown {},
    Integer {
        value: i32,
    },
    Number {
        value: f64,
    },
    #[serde(rename = "boolean")]
    Bool {
        value: bool,
    },
    Identifier {
        name: String,
    },
    From {
        exp: Box<Exp>,
        field: String,
    },
    Get {
        exp: Box<Exp>,
        field: String,
    },
    #[serde(rename = "string")]
    Stringg {
        value: String,
    },
    Undefined {},
    Unit {},
    #[serde(rename = "binop")]
    BinOp {
        op: Op2,
        e1: Box<Exp>,
        e2: Box<Exp>,
    },
    #[serde(rename = "op1")]
    Op1 {
        op: Op1,
        e: Box<Exp>,
    },
    If {
        cond: Box<Exp>,
        #[serde(rename = "truePart")]
        true_part: Vec<Exp>,
        #[serde(rename = "falsePart")]
        false_part: Vec<Exp>,
    },
    While {
        cond: Box<Exp>,
        body: Vec<Exp>,
    },
    Let {
        name: String,
        #[serde(default)]
        typ: Option<Typ>,
        named: Box<Exp>,
    },
    Set {
        name: LVal,
        named: Box<Exp>,
    },
    Block {
        body: Vec<Exp>,
    },
    Callback {
        event: String,
        #[serde(rename = "eventArg")]
        event_arg: Box<Exp>,
        #[serde(rename = "callbackArgs", deserialize_with = "deserialize_args")]
        callback_args: Vec<Arg>,
        #[serde(rename = "clos")]
        callback_clos: Box<Exp>,
        body: Vec<Exp>,
    },
    #[serde(skip)]
    Loopback {
        event: String,
        event_arg: Box<Exp>,
        callback_clos: Box<Exp>,
        id: i32,
    },
    Label {
        name: String,
        body: Vec<Exp>,
    },
    Break {
        name: String,
        value: Box<Exp>,
    },
    Object {
        properties: HashMap<String, Exp>,
    },
    Clos {
        tenv: HashMap<String, Exp>,
    },
    Array {
        exps: Vec<Exp>,
    },
    Index {
        #[serde(rename = "exp")]
        e1: Box<Exp>,
        #[serde(rename = "index")]
        e2: Box<Exp>,
    },
    #[serde(skip)]
    Ref {
        e: Box<Exp>,
    },
    #[serde(skip)]
    Deref {
        e: Box<Exp>,
    },
    #[serde(skip)]
    SetRef {
        e1: Box<Exp>,
        e2: Box<Exp>,
    },
    PrimApp {
        event: String,
        #[serde(rename = "eventArgs")]
        event_args: Vec<Exp>,
    },
    MethodCall {
        e: Box<Exp>,
        method: String,
        #[serde(rename = "methodCallArgs")]
        method_call_args: Vec<Exp>,
    },
}

#[derive(PartialEq, Debug, Deserialize, Clone)]
#[serde(tag = "kind", rename_all = "camelCase")]
pub enum LVal {
    Identifier { name: String },
    From { exp: Box<Exp>, field: String },
    Index { exp: Box<Exp>, index: Box<Exp> },
}

// https://stackoverflow.com/a/42661287
fn vec_to_string(v: &[Exp]) -> String {
    return v
        .iter()
        .fold(String::new(), |acc, num| acc + &num.to_string() + ",\n");
}

impl fmt::Display for Exp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Exp::Unknown {} => write!(f, "Unknown"),
            Exp::Integer { value } => write!(f, "Integer({})", value),
            Exp::Number { value } => write!(f, "Number({})", value),
            Exp::Bool { value } => write!(f, "Bool({})", value),
            Exp::Identifier { name } => write!(f, "Identifier({})", name),
            Exp::From { exp, field } => write!(f, "From({},{})", exp, field),
            Exp::Get { exp, field } => write!(f, "Get({},{})", exp, field),
            Exp::Stringg { value } => write!(f, "Stringg({})", value),
            Exp::Undefined {} => write!(f, "Undefined"),
            Exp::Unit {} => write!(f, "()"),
            Exp::BinOp { op, e1, e2 } => write!(f, "BinOp({:?}, {}, {})", op, e1, e2),
            Exp::Op1 { op, e } => write!(f, "Op1({:?}, {})", op, e),
            Exp::If {
                cond,
                true_part,
                false_part,
            } => write!(
                f,
                "If({} \n[{}]\n else \n[{}]\n)",
                cond,
                vec_to_string(true_part),
                vec_to_string(false_part)
            ),
            Exp::While { cond, body } => write!(f, "While({}, {:?})", cond, body),
            Exp::Let { name, typ, named } => write!(f, "Let({} : {:?}, {})", name, typ, named),
            Exp::Set { name, named } => write!(f, "Set({:?}, {})", name, named),
            Exp::Block { body } => write!(f, "Block(\n[{}])", vec_to_string(body)),
            Exp::Callback {
                event,
                event_arg,
                callback_args,
                callback_clos,
                body,
            } => write!(
                f,
                "Callback({}, {}, {:?}, {}, {})",
                event,
                event_arg,
                callback_args,
                callback_clos,
                vec_to_string(body)
            ),
            Exp::Loopback {
                event,
                event_arg,
                callback_clos,
                id,
            } => write!(
                f,
                "Loopback({}, {}, {}, {})",
                event, event_arg, callback_clos, id
            ),
            Exp::Label { name, body } => write!(f, "Label({}, {})", name, vec_to_string(body)),
            Exp::Break { name, value } => write!(f, "Break({}, {})", name, value),
            Exp::Object { properties } => write!(f, "Obj({:?}", properties),
            Exp::Clos { tenv } => write!(f, "clos({:?}", tenv),
            Exp::Array { exps } => write!(f, "Array({})", vec_to_string(exps)),
            Exp::Index { e1, e2 } => write!(f, "Index({}, {})", e1, e2),
            Exp::Ref { e } => write!(f, "Ref({})", e),
            Exp::Deref { e } => write!(f, "Deref({})", e),
            Exp::SetRef { e1, e2 } => write!(f, "SetRef({}, {})", e1, e2),
            Exp::PrimApp { event, event_args } => {
                write!(f, "PrimApp({}, {})", event, vec_to_string(event_args))
            }
            Exp::MethodCall {
                e,
                method,
                method_call_args,
            } => write!(f, "MethodCall({}, {}, {:?})", e, method, method_call_args),
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
    use super::{Arg, Exp, LVal, Op1, Op2, Typ};
    use im_rc::{HashMap as ImHashMap, HashSet as ImHashSet};
    use std::collections::HashMap;

    pub fn t_union(t1: Typ, t2: Typ) -> Typ {
        match t1 {
            Typ::Union(ts) => {
                let mut ts2 = ts;
                ts2.insert(t2);
                return Typ::Union(ts2);
            }
            _ => {
                let mut ts2 = ImHashSet::new();
                ts2.insert(t1);
                ts2.insert(t2);
                return Typ::Union(ts2);
            }
        }
    }

    pub fn t_union_2(ts: &[Typ]) -> Typ {
        let mut hs = ImHashSet::new();
        for t in ts.iter() {
            hs.insert(t.to_owned());
        }
        Typ::Union(hs)
    }

    pub fn t_obj(tm: ImHashMap<String, Typ>) -> Typ {
        Typ::Object(tm)
    }

    pub fn t_obj_2(tm: &[(&str, Typ)]) -> Typ {
        let mut hm = ImHashMap::new();
        for (k, v) in tm.iter() {
            hm.insert(k.to_string(), v.to_owned());
        }
        Typ::Object(hm)
    }

    pub fn t_ref(t: Typ) -> Typ {
        Typ::Ref(Box::new(t))
    }

    pub fn t_array(t: Typ) -> Typ {
        Typ::Array(Box::new(t))
    }

    pub fn unknown() -> Exp {
        Unknown {}
    }

    pub fn integer(value: i32) -> Exp {
        Integer { value }
    }

    pub fn number(value: f64) -> Exp {
        Number { value }
    }

    pub fn bool_(value: bool) -> Exp {
        Bool { value }
    }

    pub fn id(name: &str) -> Exp {
        Identifier {
            name: name.to_string(),
        }
    }

    pub fn from(exp: Exp, field: &str) -> Exp {
        From {
            exp: Box::new(exp),
            field: field.to_string(),
        }
    }

    pub fn get(exp: Exp, field: &str) -> Exp {
        Get {
            exp: Box::new(exp),
            field: field.to_string(),
        }
    }

    pub fn string(value: &str) -> Exp {
        Stringg {
            value: value.to_string(),
        }
    }

    pub fn undefined() -> Exp {
        Undefined {}
    }

    pub fn binop(op: &Op2, e1: Exp, e2: Exp) -> Exp {
        BinOp {
            op: op.clone(),
            e1: Box::new(e1),
            e2: Box::new(e2),
        }
    }

    pub fn op1(op: &Op1, e: Exp) -> Exp {
        Exp::Op1 {
            op: op.clone(),
            e: Box::new(e),
        }
    }

    pub fn if_(cond: Exp, true_part: Vec<Exp>, false_part: Vec<Exp>) -> Exp {
        If {
            cond: Box::new(cond),
            true_part,
            false_part,
        }
    }

    pub fn while_(cond: Exp, body: Vec<Exp>) -> Exp {
        While {
            cond: Box::new(cond),
            body,
        }
    }

    pub fn let_(name: &str, typ: Option<Typ>, named: Exp) -> Exp {
        Let {
            name: name.to_string(),
            typ,
            named: Box::new(named),
        }
    }

    pub fn set(name: LVal, named: Exp) -> Exp {
        Set {
            name,
            named: Box::new(named),
        }
    }

    pub fn ref_(e: Exp) -> Exp {
        Ref { e: Box::new(e) }
    }

    pub fn deref(e: Exp) -> Exp {
        Deref { e: Box::new(e) }
    }

    pub fn setref(e1: Exp, e2: Exp) -> Exp {
        SetRef {
            e1: Box::new(e1),
            e2: Box::new(e2),
        }
    }

    // NOTE(emily): Apparently its bad Rust to receive a Vec<T>
    // NOTE(arjun): What is the alternative?
    pub fn block(body: Vec<Exp>) -> Exp {
        Block { body }
    }

    pub fn callback(
        event: &str, event_arg: Exp, callback_args: Vec<Arg>, callback_clos: Exp, body: Vec<Exp>,
    ) -> Exp {
        Callback {
            event: event.to_string(),
            event_arg: Box::new(event_arg),
            callback_args,
            callback_clos: Box::new(callback_clos),
            body,
        }
    }

    pub fn arg(name: &str, typ: Option<Typ>) -> Arg {
        Arg {
            name: name.to_string(),
            typ,
        }
    }

    pub fn loopback(event: &str, event_arg: Exp, callback_clos: Exp, id: i32) -> Exp {
        Loopback {
            event: event.to_string(),
            callback_clos: Box::new(callback_clos),
            event_arg: Box::new(event_arg),
            id,
        }
    }

    pub fn label(name: &str, body: Vec<Exp>) -> Exp {
        Label {
            name: name.to_string(),
            body,
        }
    }

    pub fn break_(name: &str, value: Exp) -> Exp {
        Break {
            name: name.to_string(),
            value: Box::new(value),
        }
    }

    pub fn array(exps: Vec<Exp>) -> Exp {
        Array { exps }
    }

    pub fn index_(e1: Exp, e2: Exp) -> Exp {
        Index {
            e1: Box::new(e1),
            e2: Box::new(e2),
        }
    }

    pub fn prim_app(event: &str, event_args: Vec<Exp>) -> Exp {
        PrimApp {
            event: event.to_string(),
            event_args,
        }
    }

    pub fn method_call(e: Exp, method: &str, method_call_args: Vec<Exp>) -> Exp {
        MethodCall {
            e: Box::new(e),
            method: method.to_string(),
            method_call_args,
        }
    }

    pub fn obj(tenv: HashMap<String, Exp>) -> Exp {
        Object { properties: tenv }
    }

    pub fn obj_2(tenv: &[(&str, Exp)]) -> Exp {
        let mut hm = HashMap::new();
        for (k, v) in tenv.iter() {
            hm.insert(k.to_string(), v.to_owned());
        }
        Object { properties: hm }
    }

    pub fn lval_index(exp: Exp, index: Exp) -> LVal {
        LVal::Index {
            exp: Box::new(exp),
            index: Box::new(index),
        }
    }
}
