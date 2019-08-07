
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
            return LVal::From{
                exp: Box::new(transform_exp(exp)),
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
                let cond = binop(&StrictEq, string(CBID), number(x));
                let true_part = transform_exps(body);
                let mut false_part = vec!(loopback(event, transform_exp(event_arg), x));
                if(i < exps.len()-1) {
                    let (prev, rest) = exps.split_at(i+1);
                    false_part.append(&mut transform_exps(rest));
                }
                return vec!(if_(cond, true_part, false_part));
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
        Unknown { } => return unknown(),
        Number { value } => return number(*value),
        Identifier { name } => return id(name),
        From { exp, field } => return from(transform_exp(exp), field),
        Stringg { value } => return string(value),
        Undefined { } => return undefined(),
        BinOp { op, e1, e2 } => return binop(op, transform_exp(e1), transform_exp(e2)),
        If { cond, true_part, false_part } => return if_(transform_exp(cond), transform_exps(true_part), transform_exps(false_part)),
        While { cond, body } => return while_(transform_exp(cond), transform_exp(body)),
        Let { name, named } => return let_(name, transform_exp(named)),
        Set { name, named } => return set(transform_lval(name), transform_exp(named)),
        Block { body } => return block(transform_exps(body)),
        Callback { event, event_arg, callback_args, callback_clos, body } => {
            panic!("Did not expect to see a callback here.");
        }
        Label { name, body } => return label(name, transform_exps(body)),
        Break { name, value } => return break_(name, transform_exp(value)),
        Clos { tenv } => {
            return Clos {
                tenv: transform_hashmap(tenv)
            };
        },
        Array { exps } => return array(transform_exps(exps)),
        PrimApp { event, event_args } => return prim_app(event, transform_exps(event_args)),
        _ => {
            panic!("Not implemented.");
        }
    }
}

