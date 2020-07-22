use std::collections::HashMap;

use super::types::{constructors::*, Arg, Exp, Exp::*, LVal, Op2::*};

pub struct LiftCallbacks {
    cbid: String,
    cbargs: String,
    next_id: i32,
    callbacks: Vec<(i32, Vec<Exp>)>,
}

impl LiftCallbacks {
    pub fn new() -> LiftCallbacks {
        return LiftCallbacks {
            cbid: "arg_cbid".to_string(),
            cbargs: "arg_cbargs".to_string(),
            next_id: 1,
            callbacks: vec![],
        };
    }

    fn fresh_id(&mut self) -> i32 {
        self.next_id += 1;
        return self.next_id - 1;
    }

    fn lift_callback(
        &mut self, event: &str, event_arg: &Exp, callback_args: &[Arg], callback_clos: &Exp,
        body: &[Exp],
    ) -> Exp {
        let x = self.fresh_id();
        let mut lifted_body: Vec<Exp> = vec![];
        for (i, Arg { name }) in callback_args.iter().enumerate() {
            lifted_body.push(let_(
                name,
                ref_(index_(id(&self.cbargs), integer(i as i32))),
            ));
        }
        lifted_body.append(&mut self.lift_exps(body));
        self.callbacks.push((x, lifted_body));
        let loopback_call = loopback(
            event,
            self.lift_exp(event_arg),
            self.lift_exp(callback_clos),
            x,
        );
        return loopback_call;
    }

    fn lift_exps(&mut self, exps: &[Exp]) -> Vec<Exp> {
        let mut ret: Vec<Exp> = vec![];
        for (i, e) in exps.iter().enumerate() {
            match e {
                Callback {
                    event,
                    event_arg,
                    callback_args,
                    callback_clos,
                    body,
                } => {
                    if i < exps.len() - 1 {
                        let (_p, r) = exps.split_at(i + 1);
                        let mut loopback = vec![self.lift_callback(
                            event,
                            event_arg,
                            callback_args,
                            callback_clos,
                            body,
                        )];
                        loopback.append(&mut self.lift_exps(r));
                        return loopback;
                    } else {
                        let loopback = vec![self.lift_callback(
                            event,
                            event_arg,
                            callback_args,
                            callback_clos,
                            body,
                        )];
                        return loopback;
                    }
                }
                _ => {
                    let e = self.lift_exp(e);
                    ret.push(e);
                }
            }
        }
        return ret;
    }

    fn lift_clos(&mut self, exps: &HashMap<String, Exp>) -> HashMap<String, Exp> {
        let mut ret: HashMap<String, Exp> = HashMap::new();
        for (key, value) in exps.iter() {
            ret.insert(key.to_string(), self.lift_exp(value));
        }
        return ret;
    }

    fn lift_exp(&mut self, exp: &Exp) -> Exp {
        match exp {
            Unknown {} => return unknown(),
            Number { value } => return number(*value),
            Bool { value } => return bool_(*value),
            Identifier { name } => return id(name),
            From { exp: _, field: _ } => panic!("Exp::From should be eliminated"),
            Index { e1, e2 } => return index_(self.lift_exp(e1), self.lift_exp(e2)),
            Get { exp, field } => return get(self.lift_exp(exp), field),
            Stringg { value } => return string(value),
            Undefined {} => return undefined(),
            BinOp { op, e1, e2 } => return binop(op, self.lift_exp(e1), self.lift_exp(e2)),
            Op1 { op, e } => return op1(op, self.lift_exp(e)),
            If {
                cond,
                true_part,
                false_part,
            } => {
                return if_(
                    self.lift_exp(cond),
                    self.lift_exps(true_part),
                    self.lift_exps(false_part),
                )
            }
            While { cond, body } => return while_(self.lift_exp(cond), self.lift_exps(body)),
            Let { name, named } => return let_(name, self.lift_exp(named)),
            Block { body } => return block(self.lift_exps(body)),
            Callback {
                event,
                event_arg,
                callback_args,
                callback_clos,
                body,
            } => return self.lift_callback(event, event_arg, callback_args, callback_clos, body),
            Label { name, body } => return label(name, self.lift_exps(body)),
            Break { name, value } => return break_(name, self.lift_exp(value)),
            Object { properties } => return obj(self.lift_clos(properties)),
            Array { exps } => return array(self.lift_exps(exps)),
            PrimApp { event, event_args } => return prim_app(event, self.lift_exps(event_args)),
            Ref { e } => return ref_(self.lift_exp(e)),
            Deref { e } => return deref(self.lift_exp(e)),
            SetRef { e1, e2 } => return setref(self.lift_exp(e1), self.lift_exp(e2)),
            MethodCall {
                e,
                method,
                method_call_args,
            } => return method_call(self.lift_exp(e), method, self.lift_exps(method_call_args)),
            Set {
                name: LVal::Index { exp, index },
                named,
            } => set(
                lval_index(self.lift_exp(exp), self.lift_exp(index)),
                self.lift_exp(named),
            ),
            Set { name: _, named: _ } => panic!("Did not expect to find this here."),
            Loopback {
                event: _,
                event_arg: _,
                callback_clos: _,
                id: _,
            } => panic!("Did not expect to find this here."),
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
        let t = self.callbacks.iter().fold(base, |acc, (k, v)| {
            if_(
                binop(&StrictEq, id(&self.cbid), integer(*k)),
                // Not sure how to get around clone here.
                (*v).clone(),
                vec![acc],
            )
        });
        //println!("{:?}", self.callbacks);
        return t;
    }
}
