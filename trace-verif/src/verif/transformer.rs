
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
    callbacks: Vec<(i32, Vec<Exp>)>
    //callback_control_flow: Exp
}

impl Transformer {

    pub fn new() -> Transformer {
        return Transformer {
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

    fn transform_callback(&mut self, event: &str, event_arg: &Exp, callback_args: &[String], callback_clos: &Exp, body: &[Exp]) -> Exp {
        let x = self.fresh_id();
        let mut lifted_body: Vec<Exp> = vec!();
        for (i, e) in callback_args.iter().enumerate() {
            lifted_body.push(let_(e, index(id(&self.cbargs), integer(i as i32))));
        }
        lifted_body.append(&mut self.transform_exps(body));
        self.callbacks.push((x, lifted_body));
        let loopback_call = loopback(event, self.transform_exp(event_arg), self.transform_exp(callback_clos), x);
        return loopback_call;
    }

    fn transform_exps(&mut self, exps: &[Exp]) -> Vec<Exp> {
        let mut ret: Vec<Exp> = vec!();
        for (i, e) in exps.iter().enumerate() {
            match e {
                Callback { event, event_arg, callback_args, callback_clos, body } => {
                    if(i < exps.len()-1) {
                        let (p, r) = exps.split_at(i+1);
                        let mut loopback = vec!(self.transform_callback(event, event_arg, callback_args, callback_clos, body));
                        loopback.append(&mut self.transform_exps(r));
                        return loopback;
                    } else {
                        let loopback = vec!(self.transform_callback(event, event_arg, callback_args, callback_clos, body));
                        return loopback;
                    }
                },
                _ => {
                    let e = self.transform_exp(e);
                    ret.push(e);
                }
            }
        }
        return ret;
    }

    fn transform_clos(&mut self, exps: &HashMap<String,Exp>) -> HashMap<String,Exp> {
        let mut ret: HashMap<String,Exp> = HashMap::new();
        for (key, value) in exps.iter() {
            let new_value = match(value) {
                Identifier { name } => id(name),
                _ => self.transform_exp(value)
            };
            ret.insert(
                key.to_string(),
                new_value
            );
        }
        return ret;
    }

    fn transform_exp(&mut self, exp: &Exp) -> Exp {
        match exp {
            Unknown { } => return unknown(),
            Number { value } => return number(*value),
            Identifier { name } => return deref(id(name)),
            From { exp, field } => return from(self.transform_exp(exp), field),
            Stringg { value } => return string(value),
            Undefined { } => return undefined(),
            BinOp { op, e1, e2 } => return binop(op, self.transform_exp(e1), self.transform_exp(e2)),
            If { cond, true_part, false_part } => return if_(self.transform_exp(cond), self.transform_exps(true_part), self.transform_exps(false_part)),
            While { cond, body } => return while_(self.transform_exp(cond), self.transform_exp(body)),
            Let { name, typ, named } => return let_(name, ref_(self.transform_exp(named))),
            Set { name: LVal::Identifier { name }, named } =>
                return setref(id(name), self.transform_exp(named)),
            Set { name, named } => unimplemented!(), // return set(self.transform_lval(name), self.transform_exp(named)),
            Block { body } => return block(self.transform_exps(body)),
            Callback { event, event_arg, callback_args, callback_clos, body } => return self.transform_callback(event, event_arg, callback_args, callback_clos, body),
            Label { name, body } => return label(name, self.transform_exps(body)),
            Break { name, value } => return break_(name, self.transform_exp(value)),
            Clos { tenv } => return clos(self.transform_clos(tenv)),
            Array { exps } => return ref_(ref_(array(self.transform_exps(exps)))),
            PrimApp { event, event_args } => return prim_app(event, self.transform_exps(event_args)),
            Loopback { event, event_arg, callback_clos, id }  => panic!("Did not expect to find this here."),
            Ref { e }  => panic!("Did not expect to find this here."),
            Deref { e }  => panic!("Did not expect to find this here."),
            SetRef { e1, e2 }  => panic!("Did not expect to find this here."),
            _ => {
                panic!("Not implemented.");
            }
        }
    }

    /*

        1. Lifts callbacks (Loopback)
        2. Heap allocate all local variables (Alloc)
           Dereference all their uses (Deref)

    */
    pub fn transform(&mut self, exp: &Exp) -> Exp {
        let base = self. transform_exp(exp);
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