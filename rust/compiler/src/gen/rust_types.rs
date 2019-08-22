use std::collections::HashMap;

use crate::types::{
    Typ,
    Exp,
    Exp::*,
    Arg
};

pub struct RustTypes {
    types: HashMap<Typ, usize>,
    rust_types: HashMap<usize, Typ>,
    next_type_id: usize
}

impl RustTypes {

    pub fn new() -> RustTypes {
        return RustTypes {
            types: HashMap::new(),
            rust_types: HashMap::new(),
            next_type_id: 0
        };
    }

    fn next_rust_type_id(&mut self) -> usize {
        self.next_type_id += 1;
        return self.next_type_id - 1;
    }

    fn register_type(&mut self, typ: &mut Typ) -> Typ {
        // Don't add primitives to the type map
        match typ {
            Typ::I32 => return Typ::I32,
            Typ::F64 => return Typ::F64,
            Typ::Bool => return Typ::Bool,
            Typ::String => return Typ::String,
            Typ::Unknown => return Typ::Unknown,
            Typ::Undefined => return Typ::Undefined,
            Typ::ResponseCallback => return Typ::ResponseCallback,
            Typ::Object(hm) => {
                for v in hm.iter_mut() {
                    *v = self.register_type(v);
                }
            },
            Typ::Union(ts) => {
                for t in ts.iter_mut() {
                    *t = self.register_type(t);
                }
            },
            Typ::Ref(t) => {
                return Typ::Ref(Box::new(self.register_type(t)));
            },
            _ => { }
        }

        match self.types.get(typ) {
            None => {
                let id = self.next_rust_type_id();
                self.types.insert(typ.clone(), id);
                self.rust_types.insert(id, typ.to_owned());
                return Typ::RustType(id);
            },
            Some(id) => {
                return Typ::RustType(*id);
            }
        }
    }

    fn arg(&mut self, arg: &mut Arg) {
        match arg {
            Arg { name:_, typ } => {
                match typ {
                    None => panic!("Found None typ"),
                    Some(t) => *typ = Some(self.register_type(t))
                }
            }
        }
    }

    fn tenv(&mut self, tenv: &mut HashMap<String, Exp>) {
        for (_, e) in tenv.iter_mut() {
            self.exp(e);
        }
    }

    fn exps(&mut self, exps: &mut [Exp]) {
        for e in exps.iter_mut() {
            self.exp(e);
        }
    }

    fn exp(&mut self, exp: &mut Exp) {
        match exp {
            Unknown { } => { },
            Integer { value:_ } => { },
            Number { value:_ } => { },
            Bool { value:_ } => { }
            Identifier { name:_ } => { },
            From { exp, field:_ } => {
                self.exp(exp);
            },
            Stringg { value:_ } => { },
            Undefined { } => { },
            BinOp { op:_, e1, e2 } => {
                self.exp(e1);
                self.exp(e2);
            },
            If { cond, true_part, false_part } => {
                self.exp(cond);
                self.exps(true_part);
                self.exps(false_part);
            },
            While { cond, body } => {
                self.exp(cond);
                self.exp(body);
            },
            Let { name:_, typ, named } => {
                match typ {
                    None => panic!("Found None typ"),
                    Some(t) => *typ = Some(self.register_type(t))
                }
                self.exp(named);
            },
            Set { name:_, named } => {
                self.exp(named);
            },
            Block { body } => {
                self.exps(body);
            },
            Callback { event:_, event_arg, callback_args, callback_clos, body } => {
                self.exp(event_arg);
                for arg in callback_args.iter_mut() {
                    self.arg(arg);
                }
                self.exp(callback_clos);
                self.exps(body);
            },
            Loopback { event:_, event_arg, callback_clos, id:_ } => {
                self.exp(event_arg);
                self.exp(callback_clos);
            },
            Label { name:_, body } => {
                self.exps(body);
            },
            Object { properties } => {
                self.tenv(properties);
            },
            Array { exps } => {
                self.exps(exps);
            },
            Index { e1, e2 } => {
                self.exp(e1);
                self.exp(e2);
            },
            Ref { e } => {
                self.exp(e);
            },
            Deref { e } => {
                self.exp(e);
            },
            SetRef { e1, e2 } => {
                self.exp(e1);
                self.exp(e2);
            },
            PrimApp { event:_, event_args } => {
                self.exps(event_args);
            },
            _ => unimplemented!()
        }
    }
}

// TODO(emily): Only put new type in type map if it is used 2+ times
pub fn to_rust_types(exp: &mut Exp) -> HashMap<usize, Typ> {
    let mut rts = RustTypes::new();
    rts.exp(exp);
    return rts.rust_types.to_owned();
}