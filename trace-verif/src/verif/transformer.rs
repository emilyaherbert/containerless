
use std::collections::HashMap;

use crate::verif::{
    untyped_traces::{
        Op2::*,
        Exp,
        Exp::*,
        LVal,
        constructors::*
    }
};

pub struct Transformer {
    cbid: String,
    cbargs: String,
    next_id: i32,
}

impl Transformer {
    pub fn new() -> Transformer {
        return Transformer {
            cbid: "$self.cbid".to_string(),
            cbargs: "$CBARGS".to_string(),
            next_id: 0
        };
    }

    fn fresh_id(&mut self) -> i32 {
        self.next_id = self.next_id + 1;
        return self.next_id - 1;
    }

    fn transform_lval(&mut self, lval: &LVal) -> LVal {
        match lval {
            LVal::Identifier { name } => return LVal::Identifier { name: name.to_string() },
            LVal::From { exp, field } => {
                return LVal::From{
                    exp: Box::new(self.transform(exp)),
                    field: field.to_string()
                };
            }
        }
    }

    fn transform_callback(&mut self, event: &str, event_arg: &Exp, callback_args: &[String], callback_clos: &Exp, body: &[Exp], rest: &mut Vec<Exp>) -> Exp {
        let x = self.fresh_id();
        let cond = binop(&StrictEq, string(&self.cbid), integer(x));
        let mut true_part: Vec<Exp> = vec!();
        for (i, e) in callback_args.iter().enumerate() {
            true_part.push(let_(e, index(id(&self.cbargs), integer(i as i32))));
        }
        true_part.append(&mut self.transform_exps(body));
        let mut false_part = vec!(loopback(event, self.transform(event_arg), self.transform(callback_clos), x));
        false_part.append(rest);
        return if_(cond, true_part, false_part);
    }

    fn transform_exps(&mut self, exps: &[Exp]) -> Vec<Exp> {
        let mut ret: Vec<Exp> = vec!();
        for (i, e) in exps.iter().enumerate() {
            match e {
                Callback { event, event_arg, callback_args, callback_clos, body } => {
                    if(i < exps.len()-1) {
                        let (p, r) = exps.split_at(i+1);
                        let mut rest = self.transform_exps(r);
                        return vec!(self.transform_callback(event, event_arg, callback_args, callback_clos, body, &mut rest));
                    } else {
                        return vec!(self.transform_callback(event, event_arg, callback_args, callback_clos, body, &mut vec!()));
                    }
                },
                _ => {
                    let e = self.transform(e);
                    ret.push(e);
                }
            }
        }
        return ret;
    }

    fn transform_hashmap(&mut self, exps: &HashMap<String,Exp>) -> HashMap<String,Exp> {
        let mut ret: HashMap<String,Exp> = HashMap::new();
        for (key, value) in exps {
            ret.insert(
                key.to_string(),
                self.transform(value)
            );
        }
        return ret;
    }

    /*

        1. Lifts callbacks (Loopback)
        2. Heap allocate all local variables (Ref)
           Dereference all their uses (Deref)

    */
    pub fn transform(&mut self, exp: &Exp) -> Exp {
        match exp {
            Unknown { } => return unknown(),
            Number { value } => return number(*value),
            Identifier { name } => return deref(id(name)),
            From { exp, field } => return from(self.transform(exp), field),
            Stringg { value } => return string(value),
            Undefined { } => return undefined(),
            BinOp { op, e1, e2 } => return binop(op, self.transform(e1), self.transform(e2)),
            If { cond, true_part, false_part } => return if_(self.transform(cond), self.transform_exps(true_part), self.transform_exps(false_part)),
            While { cond, body } => return while_(self.transform(cond), self.transform(body)),
            Let { name, typ, named } => return let_(name, ref_(self.transform(named))),
            Set { name, named } => return set(self.transform_lval(name), self.transform(named)),
            Block { body } => return block(self.transform_exps(body)),
            Callback { event, event_arg, callback_args, callback_clos, body } => return self.transform_callback(event, event_arg, callback_args, callback_clos, body, &mut vec!()),
            Label { name, body } => return label(name, self.transform_exps(body)),
            Break { name, value } => return break_(name, self.transform(value)),
            Clos { tenv } => return clos(self.transform_hashmap(tenv)),
            Array { exps } => return array(self.transform_exps(exps)),
            PrimApp { event, event_args } => return prim_app(event, self.transform_exps(event_args)),
            _ => {
                panic!("Not implemented.");
            }
        }
    }
}