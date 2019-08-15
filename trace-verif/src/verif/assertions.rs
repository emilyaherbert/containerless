
use std::collections::{ HashMap, HashSet };

use crate::verif::{
    untyped_traces::{
        Exp,
        Exp::*,
        Arg
    }
};

pub struct Assertions {
    names: HashSet<String>
}

impl Assertions {

    pub fn new() -> Assertions {
        return Assertions {
            names: HashSet::new()
        };
    }

    fn assert_stmts(&self, exps: &[Exp]) {
        for e in exps.iter() {
            self.assert_stmt(e);
        }
    }

    fn assert_exps(&self, exps: &[Exp]) {
        for e in exps.iter() {
            self.assert_exp(e);
        }
    }

    fn assert_tenv(&self, hm: &HashMap<String, Exp>) {
        for (_k, v) in hm.iter() {
            self.assert_exp(v);
        }
    }

    fn assert_exp(&self, exp: &Exp) {
        match exp {
            Number { value:_ } => { },
            Identifier { name:_ } => { },
            From { exp, field:_ } => {
                self.assert_exp(exp);
            },
            Stringg { value:_ } => { },
            Undefined { } => { },
            BinOp { op:_, e1, e2 } => { 
                self.assert_exp(e1);
                self.assert_exp(e2);
            },
            Block { body } => {
                self.assert_stmts(body);
            },
            Clos { tenv } => {
                self.assert_tenv(tenv);
            },
            Array { exps } => {
                self.assert_exps(exps);
            },
            Ref { e }  => {
                self.assert_exp(e);
            },
            Deref { e }  => {
                self.assert_exp(e);
            },
            _ => {
                panic!("Did not expect to find {:?} here.", exp);
            }
        }
    }

    fn assert_stmt(&self, exp: &Exp) {
        match exp {
            Unknown { } => { },
            If { cond, true_part, false_part } => {
                self.assert_exp(cond);
                self.assert_stmts(true_part);
                self.assert_stmts(false_part);
            },
            While { cond, body } => {
                self.assert_exp(cond);
                self.assert_exp(body);
            },
            Let { name:_, typ:_, named } => {
                self.assert_exp(named);
            },
            Set { name:_, named } => {
                self.assert_exp(named);
            },
            SetRef { e1, e2 }  => {
                self.assert_exp(e1);
                self.assert_exp(e2);
            },
            Block { body } => {
                self.assert_stmts(body);
            },
            Callback { event:_, event_arg, callback_args:_, callback_clos, body } => {
                self.assert_exp(event_arg);
                self.assert_exp(callback_clos);
                self.assert_stmts(body);
            },
            Label { name:_, body } => {
                self.assert_stmts(body);
            },
            Break { name:_, value } => {
                self.assert_exp(value);
            },
            PrimApp { event:_, event_args } => {
                self.assert_exps(event_args);
            },
            Loopback { event:_, event_arg, callback_clos, id:_ }  => {
                self.assert_exp(event_arg);
                self.assert_exp(callback_clos);
            },
            _ => {
                panic!("Did not expect to find {:?} here.", exp);
            }
        }
    }

    pub fn assert_supposed_grammar(&self, exp: &Exp) {
        self.assert_exp(exp);
    }

    fn assert_unique_names_vec(&mut self, exps: &[Exp]) {
        for e in exps.iter() {
            self.assert_unique_names(e);
        }
    }

    fn assert_unique_names_tenv(&mut self, exps: &HashMap<String, Exp>) {
        for (_k, v) in exps.iter() {
            self.assert_unique_names(v);
        }
    }

    pub fn assert_unique_names(&mut self, exp: &Exp) {
        match exp {
            Unknown { } => { },
            Number { value:_ } => { },
            Identifier { name:_ } => { },
            From { exp, field:_ } => {
                self.assert_unique_names(exp);
            },
            Stringg { value:_ } => { },
            Undefined { } => { },
            BinOp { op:_, e1, e2 } => {
                self.assert_unique_names(e1);
                self.assert_unique_names(e2);
            },
            If { cond:_, true_part, false_part } => {
                self.assert_unique_names_vec(true_part);
                self.assert_unique_names_vec(false_part);
            }
            While { cond:_, body } => {
                self.assert_unique_names(body);
            }
            Let { name, typ:_, named:_ } => {
                let first = name.chars().next().unwrap();
                if first != '$' {
                    if self.names.contains(name) {
                        panic!("Found non-unique name!");
                    } else {
                        self.names.insert(name.to_string());
                    }
                }
            },
            Set { name:_, named } => {
                self.assert_unique_names(named);
            },
            Block { body } => {
                self.assert_unique_names_vec(body);
            }
            Callback { event:_, event_arg, callback_args:_, callback_clos, body } => {
                self.assert_unique_names(event_arg);
                self.assert_unique_names(callback_clos);
                self.assert_unique_names_vec(body); 
            },
            Label { name:_, body } => {
                self.assert_unique_names_vec(body);
            },
            Break { name:_, value } => {
                self.assert_unique_names(value);
            }
            Clos { tenv } => {
                self.assert_unique_names_tenv(tenv);
            },
            Array { exps } => {
                self.assert_unique_names_vec(exps);
            },
            PrimApp { event:_, event_args } => {
                self.assert_unique_names_vec(event_args);
            },
            Loopback { event:_, event_arg, callback_clos, id:_ } => {
                self.assert_unique_names(event_arg);
                self.assert_unique_names(callback_clos);
            },
            Ref { e }  => {
                self.assert_unique_names(e);
            },
            Deref { e }  => {
                self.assert_unique_names(e);
            }
            SetRef { e1, e2 }  => {
                self.assert_unique_names(e1);
                self.assert_unique_names(e2);
            }
            _ => {
                panic!("Not implemented.");
            }
        }
    }

    fn assert_all_options_are_none_vec(&mut self, exps: &[Exp]) {
        for e in exps.iter() {
            self.assert_all_options_are_none(e);
        }
    }

    fn assert_all_options_are_none_tenv(&mut self, exps: &HashMap<String, Exp>) {
        for (_k, v) in exps.iter() {
            self.assert_all_options_are_none(v);
        }
    }

    pub fn assert_all_options_are_none(&mut self, exp: &Exp) {
        match exp {
            Unknown { } => { },
            Number { value:_ } => { },
            Identifier { name:_ } => { },
            From { exp, field:_ } => {
                self.assert_all_options_are_none(exp);
            },
            Stringg { value:_ } => { },
            Undefined { } => { },
            BinOp { op:_, e1, e2 } => {
                self.assert_all_options_are_none(e1);
                self.assert_all_options_are_none(e2);
            },
            If { cond:_, true_part, false_part } => {
                self.assert_all_options_are_none_vec(true_part);
                self.assert_all_options_are_none_vec(false_part);
            }
            While { cond:_, body } => {
                self.assert_all_options_are_none(body);
            }
            Let { name:_, typ, named } => {
                match typ {
                    Some(_) => panic!("Not Some"),
                    None => {
                        self.assert_all_options_are_none(named);
                    }
                }
            },
            Set { name:_, named } => {
                self.assert_all_options_are_none(named);
            },
            Block { body } => {
                self.assert_all_options_are_none_vec(body);
            }
            Callback { event:_, event_arg, callback_args, callback_clos, body } => {
                for Arg { name:_, typ } in callback_args.iter() {
                    match typ {
                        Some(_) => panic!("Found some"),
                        None => {}
                    }
                }
                self.assert_all_options_are_none(event_arg);
                self.assert_all_options_are_none(callback_clos);
                self.assert_all_options_are_none_vec(body); 
            },
            Label { name:_, body } => {
                self.assert_all_options_are_none_vec(body);
            },
            Break { name:_, value } => {
                self.assert_all_options_are_none(value);
            }
            Clos { tenv } => {
                self.assert_all_options_are_none_tenv(tenv);
            },
            Array { exps } => {
                self.assert_all_options_are_none_vec(exps);
            },
            PrimApp { event:_, event_args } => {
                self.assert_all_options_are_none_vec(event_args);
            },
            Loopback { event:_, event_arg, callback_clos, id:_ } => {
                self.assert_all_options_are_none(event_arg);
                self.assert_all_options_are_none(callback_clos);
            },
            Ref { e }  => {
                self.assert_all_options_are_none(e);
            },
            Deref { e }  => {
                self.assert_all_options_are_none(e);
            }
            SetRef { e1, e2 }  => {
                self.assert_all_options_are_none(e1);
                self.assert_all_options_are_none(e2);
            }
            _ => {
                panic!("Not implemented.");
            }
        }
    }
}