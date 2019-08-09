
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

pub struct Assertions {

}

impl Assertions {

    pub fn new() -> Assertions {
        return Assertions { };
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
        for (k, v) in hm.iter() {
            self.assert_exp(v);
        }
    }

    fn assert_exp(&self, exp: &Exp) {
        match exp {
            Number { value } => { },
            Identifier { name } => { },
            From { exp, field } => {
                self.assert_exp(exp);
            },
            Stringg { value } => { },
            Undefined { } => { },
            BinOp { op, e1, e2 } => { 
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
            Let { name, typ, named } => {
                self.assert_exp(named);
            },
            Set { name, named } => {
                self.assert_exp(named);
            },
            SetRef { e1, e2 }  => {
                self.assert_exp(e1);
                self.assert_exp(e2);
            },
            Block { body } => {
                self.assert_stmts(body);
            },
            Callback { event, event_arg, callback_args, callback_clos, body } => {
                self.assert_exp(event_arg);
                self.assert_exp(callback_clos);
                self.assert_stmts(body);
            },
            Label { name, body } => {
                self.assert_stmts(body);
            },
            Break { name, value } => {
                self.assert_exp(value);
            },
            PrimApp { event, event_args } => {
                self.assert_exps(event_args);
            },
            Loopback { event, event_arg, callback_clos, id }  => {
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
}