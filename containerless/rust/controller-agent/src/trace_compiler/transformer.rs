use std::collections::HashMap;

use super::types::{constructors::*, Exp, Exp::*, LVal};

pub struct Transformer {}

impl Transformer {
    pub fn new() -> Transformer {
        return Transformer {};
    }

    fn transform_exps(&mut self, exps: &[Exp]) -> Vec<Exp> {
        let mut ret: Vec<Exp> = vec![];
        for e in exps.iter() {
            ret.push(self.transform_exp(e));
        }
        return ret;
    }

    fn transform_clos(&mut self, exps: &HashMap<String, Exp>) -> HashMap<String, Exp> {
        let mut ret: HashMap<String, Exp> = HashMap::new();
        for (key, value) in exps.iter() {
            let new_value = match value {
                Identifier { name } => id(name),
                From { exp, field } => get(self.transform_exp(exp), field),
                _ => self.transform_exp(value),
            };
            ret.insert(key.to_string(), new_value);
        }
        return ret;
    }

    fn transform_exp(&mut self, exp: &Exp) -> Exp {
        match exp {
            Unknown {} => return unknown(),
            Number { value } => return number(*value),
            Bool { value } => return bool_(*value),
            Identifier { name } => return deref(id(name)),
            Get { exp, field } => get(self.transform_exp(exp), field),
            From { exp, field } => return deref(get(self.transform_exp(exp), field)),
            Index { e1, e2 } => return index_(self.transform_exp(e1), self.transform_exp(e2)),
            Stringg { value } => return string(value),
            Undefined {} => return undefined(),
            BinOp { op, e1, e2 } => {
                return binop(op, self.transform_exp(e1), self.transform_exp(e2))
            }
            Op1 { op, e } => op1(op, self.transform_exp(e)),
            If {
                cond,
                true_part,
                false_part,
            } => {
                return if_(
                    self.transform_exp(cond),
                    self.transform_exps(true_part),
                    self.transform_exps(false_part),
                );
            }
            While { cond, body } => {
                return while_(self.transform_exp(cond), self.transform_exps(body))
            }
            Let { name, named } => {
                return let_(name, ref_(self.transform_exp(named)))
            }
            Set {
                name: LVal::Identifier { name },
                named,
            } => return setref(id(name), self.transform_exp(named)),
            Set {
                name: LVal::From { exp, field },
                named,
            } => {
                return setref(
                    get(self.transform_exp(exp), field),
                    self.transform_exp(named),
                );
            }
            Set {
                name: LVal::Index { exp, index },
                named,
            } => {
                return set(
                    lval_index(self.transform_exp(exp), self.transform_exp(index)),
                    self.transform_exp(named),
                );
            }
            Block { body } => return block(self.transform_exps(body)),
            Callback {
                event,
                event_arg,
                callback_args,
                callback_clos,
                body,
            } => {
                return callback(
                    event,
                    self.transform_exp(event_arg),
                    callback_args.to_vec(),
                    self.transform_exp(callback_clos),
                    self.transform_exps(body),
                );
            }
            Label { name, body } => return label(name, self.transform_exps(body)),
            Break { name, value } => return break_(name, self.transform_exp(value)),
            Clos { tenv } => return obj(self.transform_clos(tenv)),
            Object { properties } => {
                let mut ret: HashMap<String, Exp> = HashMap::new();
                for (key, value) in properties.iter() {
                    ret.insert(key.to_string(), self.transform_exp(value));
                }
                return obj(ret);
            }
            Array { exps } => return array(self.transform_exps(exps)),
            PrimApp { event, event_args } => {
                return prim_app(event, self.transform_exps(event_args));
            }
            MethodCall {
                e,
                method,
                method_call_args,
            } => {
                return method_call(
                    self.transform_exp(e),
                    method,
                    self.transform_exps(method_call_args),
                );
            }
            Loopback {
                event: _,
                event_arg: _,
                callback_clos: _,
                id: _,
            } => panic!("Did not expect to find this here."),
            Ref { e: _ } => panic!("Did not expect to find this here."),
            Deref { e: _ } => panic!("Did not expect to find this here."),
            SetRef { e1: _, e2: _ } => panic!("Did not expect to find this here."),
            _ => {
                panic!("Not implemented.");
            }
        }
    }

    /*

        1. Heap allocate all local variables (Ref)
        2. Dereference all their uses (Deref)
        3. Transform Set to SetRef

    */
    pub fn transform(&mut self, exp: &Exp) -> Exp {
        return self.transform_exp(exp);
    }
}
