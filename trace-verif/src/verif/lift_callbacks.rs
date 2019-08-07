
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
    next_id: f64,
}

impl Transformer {
    pub fn new() -> Transformer {
        return Transformer {
            cbid: "$self.cbid".to_string(),
            cbargs: "$CBARGS".to_string(),
            next_id: 0.0
        };
    }

    fn fresh_id(&mut self) -> f64 {
        self.next_id = self.next_id + 1.0;
        return self.next_id - 1.0;
    }

    fn transform_lval(&mut self, lval: &LVal) -> LVal {
        match lval {
            LVal::Identifier { name } => return LVal::Identifier { name: name.to_string() },
            LVal::From { exp, field } => {
                return LVal::From{
                    exp: Box::new(self.transform_exp(exp)),
                    field: field.to_string()
                };
            }
        }
    }

    fn transform_exps(&mut self, exps: &[Exp]) -> Vec<Exp> {
        let mut ret: Vec<Exp> = vec!();
        for (i, e) in exps.iter().enumerate() {
            match e {
                Callback { event, event_arg, callback_args, callback_clos, body } => {
                    let x = self.fresh_id();
                    let cond = binop(&StrictEq, string(&self.cbid), number(x));
                    let true_part = self.transform_exps(body); // TODO(emily): fix
                    let mut false_part = vec!(loopback(event, self.transform_exp(event_arg), self.transform_exp(callback_clos), x));
                    if(i < exps.len()-1) {
                        let (prev, rest) = exps.split_at(i+1);
                        false_part.append(&mut self.transform_exps(rest));
                    }
                    return vec!(if_(cond, true_part, false_part));
                },
                _ => {
                    let e = self.transform_exp(e);
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
                self.transform_exp(value)
            );
        }
        return ret;
    }

    pub fn transform_exp(&mut self, exp: &Exp) -> Exp {
        match exp {
            Unknown { } => return unknown(),
            Number { value } => return number(*value),
            Identifier { name } => return id(name),
            From { exp, field } => return from(self.transform_exp(exp), field),
            Stringg { value } => return string(value),
            Undefined { } => return undefined(),
            BinOp { op, e1, e2 } => return binop(op, self.transform_exp(e1), self.transform_exp(e2)),
            If { cond, true_part, false_part } => return if_(self.transform_exp(cond), self.transform_exps(true_part), self.transform_exps(false_part)),
            While { cond, body } => return while_(self.transform_exp(cond), self.transform_exp(body)),
            Let { name, named } => return let_(name, self.transform_exp(named)),
            Set { name, named } => return set(self.transform_lval(name), self.transform_exp(named)),
            Block { body } => return block(self.transform_exps(body)),
            Callback { event, event_arg, callback_args, callback_clos, body } => {
                panic!("Did not expect to see a callback here.");
            }
            Label { name, body } => return label(name, self.transform_exps(body)),
            Break { name, value } => return break_(name, self.transform_exp(value)),
            Clos { tenv } => {
                return Clos {
                    tenv: self.transform_hashmap(tenv)
                };
            },
            Array { exps } => return array(self.transform_exps(exps)),
            PrimApp { event, event_args } => return prim_app(event, self.transform_exps(event_args)),
            _ => {
                panic!("Not implemented.");
            }
        }
    }
}