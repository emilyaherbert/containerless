
use std::collections::HashMap;

use crate::verif::{
    untyped_traces::{
        Op2::*,
        Exp,
        Exp::*,
        LVal
    }
};

const CBID: &str = "$CBID";
const CBARGS: &str = "$CBARGS";

static mut NEXT_ID: f64 = 0.0;
fn fresh_id() -> f64 {
    unsafe {
        NEXT_ID = NEXT_ID + 1.0;
        return NEXT_ID - 1.0;
    }
}

fn transform_lval(lval: &LVal) -> LVal {
    match lval {
        LVal::Identifier { name } => return LVal::Identifier { name: name.to_string() },
        LVal::From { exp, field } => {
            let exp = transform_exp(exp);
            return LVal::From{
                exp: Box::new(exp),
                field: field.to_string()
            };
        }
    }
}

fn transform_exps(exps: &[Exp]) -> Vec<Exp> {
    let mut ret: Vec<Exp> = vec!();
    for (i, e) in exps.iter().enumerate() {
        match e {
            Callback { event, event_arg, callback_args, callback_clos, body } => {
                let x = fresh_id();
                let mut false_part = vec!(Loopback {
                    event: event.to_string(),
                    event_arg: Box::new(transform_exp(event_arg)),
                    id: x
                });
                if(i < exps.len()-1) {
                    let (prev, rest) = exps.split_at(i+1);
                    false_part.append(&mut transform_exps(rest));
                }
                return vec!(If {
                    cond: Box::new(BinOp {
                        op: StrictEq,
                        e1: Box::new(Stringg { value: CBID.to_string() }),
                        e2: Box::new(Number { value: x })
                    }),
                    true_part: transform_exps(body), // TODO(emily): Need to add [callback_clos -> CHARGS]
                    false_part: false_part
                });
            },
            _ => {
                let e = transform_exp(e);
                ret.push(e);
            }
        }
    }
    return ret;
}

fn transform_hashmap(exps: &HashMap<String,Exp>) -> HashMap<String,Exp> {
    let mut ret: HashMap<String,Exp> = HashMap::new();
    for (key, value) in exps {
        ret.insert(
            key.to_string(),
            transform_exp(value)
        );
    }
    return ret;
}

pub fn transform_exp(exp: &Exp) -> Exp {
    match exp {
        Unknown { } => return Unknown { },
        Number { value } => return Number { value: *value },
        Identifier { name } => return Identifier { name: name.to_string() },
        From { exp, field } => {
            return From {
                exp: Box::new(transform_exp(exp)),
                field: field.to_string()
            };
        },
        Stringg { value } => return Stringg { value: value.to_string() },
        Undefined { } => return Undefined { },
        BinOp { op, e1, e2 } => {
            return BinOp {
                op: (*op).clone(),
                e1: Box::new(transform_exp(e1)),
                e2: Box::new(transform_exp(e2))
            };
        },
        If { cond, true_part, false_part } => {
            return If {
                cond: Box::new(transform_exp(cond)),
                true_part: transform_exps(true_part),
                false_part: transform_exps(false_part)
            };
        },
        While { cond, body } => {
            return While {
                cond: Box::new(transform_exp(cond)),
                body: Box::new(transform_exp(body))
            };
        },
        Let { name, named } => {
            return Let {
                name: name.to_string(),
                named: Box::new(transform_exp(named))
            };
        },
        Set { name, named } => {
            return Set {
                name: transform_lval(name),
                named: Box::new(transform_exp(named))
            };
        },
        Block { body } => {
            return Block {
                body: transform_exps(body)
            };
        },
        Callback { event, event_arg, callback_args, callback_clos, body } => {
            panic!("Did not expect to see a callback here.");
        }
        Label { name, body } => {
            return Label {
                name: name.to_string(),
                body: transform_exps(body)
            };
        },
        Clos { tenv } => {
            return Clos {
                tenv: transform_hashmap(tenv)
            };
        },
        Array { exps } => {
            return Array {
                exps: transform_exps(exps)
            };
        },
        PrimApp { event, event_args } => {
            return PrimApp {
                event: event.to_string(),
                event_args: transform_exps(event_args)
            };
        }
        _ => {
            panic!("Not implemented.");
        }
    }
}

