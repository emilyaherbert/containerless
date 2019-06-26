#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused_variables)]
#![allow(unused_parens)]
#![allow(unused_must_use)]

extern crate common;

use std::collections::HashMap;

use common::{
    Env,
    TrexpAtom,
    TrexpAtom::{
        Bool,
        Int,
        Id
    },
    Trexp,
    Trexp::{
        AExp,
        Seq,
        BinOp,
        From,
        Let,
        Set,
        If,
        While,
        Label,
        Break,
        Unknown,
        Clos
    },
    Type,
    Type::{
        TBool,
        TInt,
        TUnknown,
        TClos
    }
};

fn expect_bool(ty: Type) {
    match ty {
        TBool => {},
        _ => panic!("Expected TBool."),
    }
}

fn expect_clos(ty: Type) {
    match ty {
        TClos => {},
        _ => panic!("Expected TClos."),
    }
}

fn check_aexp(env: &mut Env, aexp: &TrexpAtom) -> Type {
    match aexp {
        Bool(_) => return TBool,
        Int(_) => return TInt,
        Id(x) => {
            let ty = env.get(x)
                .unwrap_or_else(|| panic!("Id not found."));
            return *ty;
        },
    }
}

fn check_trexp(env: &mut Env, trexp: &Trexp) -> Type {
    match trexp {
        AExp(aexp) => return check_aexp(env, aexp),
        Seq(trexp1, trexp2) => {
            check_trexp(env, trexp1);
            return check_trexp(env, trexp2);
        },
        BinOp(trexp1, trexp2) => {
            let t1 = check_trexp(env, trexp1);
            let t2 = check_trexp(env, trexp2);
            if (t1 == t2) {
                return t1;
            } else {
                panic!("Mismatched BinOp types!");
            }
        },
        Let(x, trexp1, trexp2) => {
            let t1 = check_trexp(env, trexp1);
            match env.get(x) {
                Some(old) => {
                    env.insert(x.to_string(), t1).unwrap();
                    let t2 = check_trexp(env, trexp2);
                    env.insert(x.to_string(), t2).unwrap();
                    return t2;
                },
                None => {
                    env.insert(x.to_string(), t1).unwrap();
                    let t2 = check_trexp(env, trexp2);
                    env.remove(x).unwrap();
                    return t2;
                }
            }
        },
        Set(x, trexp1, trexp2) => {
            let t1 = check_trexp(env, trexp1);
            env.insert(x.to_string(), t1).unwrap();
            let t2 = check_trexp(env, trexp2);
            env.remove(x).unwrap();
            return t2;
        },
        If(trexp1, trexp2, trexp3) => {
            expect_bool(check_trexp(env, trexp1));
            let t1 = check_trexp(env, trexp2);
            let t2 = check_trexp(env, trexp3);
            if (t1 == t2) {
                return t1;
            } else {
                panic!("Mismatched If types!");
            }
        },
        While(trexp1, trexp2) => {
            expect_bool(check_trexp(env, trexp1));
            return check_trexp(env, trexp2);
        },
        Label(_, trexp) => return check_trexp(env, trexp),
        Break(_, trexp) => return check_trexp(env, trexp),
        Unknown => return TUnknown,
        Clos(trexps) => {
            trexps.iter().map(|(id, trexp)| check_trexp(env, trexp));
            return TUnknown
        }
        _ => panic!("Not implemented!!!"),
    }
}

fn main() {
    let test_trexp = BinOp(Box::new(AExp(Int(10))), Box::new(AExp(Int(1))));
    println!("{:?}", test_trexp);

    let mut env: Env = HashMap::new();

    let result = check_trexp(&mut env, &test_trexp);
    println!("{:?}", result);
}
