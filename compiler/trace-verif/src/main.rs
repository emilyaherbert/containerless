#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused_variables)]
#![allow(unused_parens)]
#![allow(unused_must_use)]

extern crate common;

use im_rc::hashmap::HashMap;

use common::types;
use types::{
    Env,
    AExp::{
        Bool,
        Int,
        Id,
        From,
    },
    CExp::{
        AExp,
        Seq,
        BinOp,
        Let,
        Set,
        If,
        While,
        Label,
        Break,
        Unknown,
        Clos
    },
    Type::{
        TBool,
        TInt,
        TUnknown,
        TClos
    }
};

fn expect_bool(ty: types::Type) {
    match ty {
        TBool => {},
        _ => panic!("Expected TBool."),
    }
}

fn check_aexp(env: &Env, aexp: &types::AExp) -> types::Type {
    match aexp {
        Bool(_) => return TBool,
        Int(_) => return TInt,
        Id(x) => {
            let ty = env.get(x)
                .unwrap_or_else(|| panic!("Id not found."));
            return ty.clone();
        },
        From(x, y) => {
            match env.get(x) {
                Some(TClos(types)) => {
                    match types.get(y) {
                        Some(ty) => return ty.clone(),
                        None => panic!("Id not in types.")
                    };
                },
                Some(_) => panic!("Expected closure!"),
                None => panic!("Expected closure!")
            }
        }
    }
}

fn check_cexp(env: &Env, cexp: &types::CExp) -> types::Type {
    match cexp {
        AExp(aexp) => return check_aexp(env, aexp),
        Seq(cexp1, cexp2) => {
            check_cexp(env, cexp1);
            return check_cexp(env, cexp2);
        },
        BinOp(cexp1, cexp2) => {
            let t1 = check_cexp(env, cexp1);
            let t2 = check_cexp(env, cexp2);
            if (t1 == t2) {
                return t1;
            } else {
                panic!("Mismatched BinOp types!");
            }
        },
        Let(x, cexp1, cexp2) => {
            let t1 = check_cexp(env, cexp1);
            let mut env2 = env.clone();
            env2.insert(x.to_string(), t1);
            return check_cexp(&env2, cexp2);
        },
        Set(x, cexp) => {
            let t = check_cexp(env, cexp);
            match env.get(x) {
                Some(other) => {
                    if(t == *other) {
                        return t;
                    } else {
                        panic!("Mismatched types!");
                    }
                },
                None => return t,
            }
        },
        If(cexp1, cexp2, cexp3) => {
            expect_bool(check_cexp(env, cexp1));
            let t1 = check_cexp(env, cexp2);
            let t2 = check_cexp(env, cexp3);
            if (t1 == t2) {
                return t1;
            } else {
                panic!("Mismatched If types!");
            }
        },
        While(cexp1, cexp2) => {
            expect_bool(check_cexp(env, cexp1));
            return check_cexp(env, cexp2);
        },
        Label(_, cexp) => return check_cexp(env, cexp),
        Break(_, cexp) => return check_cexp(env, cexp),
        Unknown => return TUnknown,
        Clos(cexps) => {
            let types: Env = cexps.iter()
                .map(|(id, cexp)| (id.to_string(), check_cexp(env, cexp)))
                .collect();
            return TClos(types);
        }
    }
}

fn main() {
    let test_cexp = BinOp(Box::new(AExp(Int(10))), Box::new(AExp(Int(1))));
    println!("{:?}", test_cexp);

    let env: Env = HashMap::new();

    let result = check_cexp(&env, &test_cexp);
    println!("{:?}", result);

}

#[test]
fn test_add() {
    let test_cexp = BinOp(Box::new(AExp(Int(10))), Box::new(AExp(Int(1))));
    let env: Env = HashMap::new();
    let result = check_cexp(&env, &test_cexp);
    assert_eq!(result, TInt);
}

#[test]
#[should_panic]
fn test_add_p() {
    let test_cexp = BinOp(Box::new(AExp(Int(10))), Box::new(AExp(Bool(false))));
    let env: Env = HashMap::new();
    let result = check_cexp(&env, &test_cexp);
    assert_eq!(result, TInt);
}

#[test]
fn test_let() {
    let test_cexp = {
        Let("x".to_string(),
            Box::new(AExp(Int(13))),
            Box::new(AExp(Id("x".to_string()))))
    };
    let env: Env = HashMap::new();
    let result = check_cexp(&env, &test_cexp);
    assert_eq!(result, TInt);
}

#[test]
fn test_set() {
    let test_cexp = {
        Let("x".to_string(),
            Box::new(AExp(Bool(false))),
            Box::new(Set("x".to_string(),
                Box::new(AExp(Bool(true))))))
    };
    let env: Env = HashMap::new();
    let result = check_cexp(&env, &test_cexp);
    assert_eq!(result, TBool);
}

#[test]
#[should_panic]
fn test_set_p() {
    let test_cexp = {
        Let("x".to_string(),
            Box::new(AExp(Int(13))),
            Box::new(Set("x".to_string(),
                Box::new(AExp(Bool(false))))))
    };
    let env: Env = HashMap::new();
    let result = check_cexp(&env, &test_cexp);
    assert_eq!(result, TInt);
}

#[test]
fn test_if() {
    let test_cexp = {
        If(Box::new(AExp(Bool(true))),
            Box::new(AExp(Int(4))),
            Box::new(AExp(Int(10))))
    };
    let env: Env = HashMap::new();
    let result = check_cexp(&env, &test_cexp);
    assert_eq!(result, TInt);
}

#[test]
#[should_panic]
fn test_if_p() {
    let test_cexp = {
        If(Box::new(AExp(Bool(true))),
            Box::new(AExp(Int(4))),
            Box::new(AExp(Bool(false))))
    };
    let env: Env = HashMap::new();
    let result = check_cexp(&env, &test_cexp);
    assert_eq!(result, TInt);
}

#[test]
fn test_while() {
    let test_cexp = {
        While(Box::new(AExp(Bool(false))),
            Box::new(Seq(Box::new(BinOp(Box::new(AExp(Int(10))), Box::new(AExp(Int(1))))),
                Box::new(AExp(Bool(false))))))
    };
    let env: Env = HashMap::new();
    let result = check_cexp(&env, &test_cexp);
    assert_eq!(result, TBool);
}

#[test]
#[should_panic]
fn test_while_p() {
    let test_cexp = {
        While(Box::new(AExp(Int(4))),
            Box::new(Seq(Box::new(BinOp(Box::new(AExp(Int(10))), Box::new(AExp(Int(1))))),
                Box::new(AExp(Bool(false))))))
    };
    let env: Env = HashMap::new();
    let result = check_cexp(&env, &test_cexp);
    assert_eq!(result, TBool);
}

#[test]
fn test_label() {
    let test_cexp = {
        Label("fitbit".to_string(),
            Box::new(Break("fitbit".to_string(),
                Box::new(Unknown))))
    };
    let env: Env = HashMap::new();
    let result = check_cexp(&env, &test_cexp);
    assert_eq!(result, TUnknown);
}