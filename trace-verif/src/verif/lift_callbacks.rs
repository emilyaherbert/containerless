
use std::collections::HashMap;

use crate::verif::{
    untyped_traces::{
        Op2::*,
        Exp,
        Exp::*,
        LVal,
        constructors::*,
        Arg
    }
};

pub struct LiftCallbacks {
    cbid: String,
    cbargs: String,
    next_id: i32,
    callbacks: Vec<(i32, Vec<Exp>)>
}

impl LiftCallbacks {

    pub fn new() -> LiftCallbacks {
        return LiftCallbacks {
            cbid: "$CBID".to_string(),
            cbargs: "$CBARGS".to_string(),
            next_id: 1,
            callbacks: vec!()
        };
    }

    fn fresh_id(&mut self) -> i32 {
        self.next_id = self.next_id + 1;
        return self.next_id - 1;
    }

    fn lift_lval(&mut self, lval: &LVal) -> LVal {
        match lval {
            LVal::Identifier { name } => return LVal::Identifier { name: name.to_string() },
            LVal::From { exp, field } => {
                return LVal::From{
                    exp: Box::new(self.lift_exp(exp)),
                    field: field.to_string()
                };
            }
        }
    }

    fn lift_callback(&mut self, event: &str, event_arg: &Exp, callback_args: &[Arg], callback_clos: &Exp, body: &[Exp]) -> Exp {
        let x = self.fresh_id();
        let mut lifted_body: Vec<Exp> = vec!();
        for (i, Arg { name, typ }) in callback_args.iter().enumerate() {
            lifted_body.push(let_(name, typ.clone(), ref_(index(id(&self.cbargs), integer(i as i32)))));
        }
        lifted_body.append(&mut self.lift_exps(body));
        self.callbacks.push((x, lifted_body));
        let loopback_call = loopback(event, self.lift_exp(event_arg), self.lift_exp(callback_clos), x);
        return loopback_call;
    }

    fn lift_exps(&mut self, exps: &[Exp]) -> Vec<Exp> {
        let mut ret: Vec<Exp> = vec!();
        for (i, e) in exps.iter().enumerate() {
            match e {
                Callback { event, event_arg, callback_args, callback_clos, body } => {
                    if(i < exps.len()-1) {
                        let (p, r) = exps.split_at(i+1);
                        let mut loopback = vec!(self.lift_callback(event, event_arg, callback_args, callback_clos, body));
                        loopback.append(&mut self.lift_exps(r));
                        return loopback;
                    } else {
                        let loopback = vec!(self.lift_callback(event, event_arg, callback_args, callback_clos, body));
                        return loopback;
                    }
                },
                _ => {
                    let e = self.lift_exp(e);
                    ret.push(e);
                }
            }
        }
        return ret;
    }

    fn lift_clos(&mut self, exps: &HashMap<String,Exp>) -> HashMap<String,Exp> {
        let mut ret: HashMap<String,Exp> = HashMap::new();
        for (key, value) in exps.iter() {
            ret.insert(
                key.to_string(),
                self.lift_exp(value)
            );
        }
        return ret;
    }

    fn lift_exp(&mut self, exp: &Exp) -> Exp {
        match exp {
            Unknown { } => return unknown(),
            Number { value } => return number(*value),
            Identifier { name } => return id(name),
            From { exp, field } => return from(self.lift_exp(exp), field),
            Stringg { value } => return string(value),
            Undefined { } => return undefined(),
            BinOp { op, e1, e2 } => return binop(op, self.lift_exp(e1), self.lift_exp(e2)),
            If { cond, true_part, false_part } => return if_(self.lift_exp(cond), self.lift_exps(true_part), self.lift_exps(false_part)),
            While { cond, body } => return while_(self.lift_exp(cond), self.lift_exp(body)),
            Let { name, typ, named } => return let_(name, typ.clone(), ref_(self.lift_exp(named))),
            Block { body } => return block(self.lift_exps(body)),
            Callback { event, event_arg, callback_args, callback_clos, body } => return self.lift_callback(event, event_arg, callback_args, callback_clos, body),
            Label { name, body } => return label(name, self.lift_exps(body)),
            Break { name, value } => return break_(name, self.lift_exp(value)),
            Clos { tenv } => return clos(self.lift_clos(tenv)),
            Array { exps } => return array(self.lift_exps(exps)),
            PrimApp { event, event_args } => return prim_app(event, self.lift_exps(event_args)),
            Ref { e }  => return ref_(self.lift_exp(e)),
            Deref { e } => return deref(self.lift_exp(e)),
            SetRef { e1, e2 } => return setref(self.lift_exp(e1), self.lift_exp(e1)),
            Set { name, named } => panic!("Did not expect to find this here."),
            Loopback { event, event_arg, callback_clos, id } => panic!("Did not expect to find this here."),
            _ => {
                panic!("Not implemented.");
            }
        }
    }

    /*

        1. Transforms callbacks to loopbacks, and lifts callback bodies to self.callbacks

    */
    pub fn lift(&mut self, exp: &Exp) -> Exp {
        let base = self.lift_exp(exp);
        let t = self.callbacks.iter()
            .fold(base, |acc, (k, v)| {
                if_(
                    binop(&StrictEq, id(&self.cbid), integer(*k)),
                    // Not sure how to get around clone here.
                    (*v).clone(),
                    vec!(acc)
                )
            });
        //println!("{:?}", self.callbacks);
        return t;
    }
}