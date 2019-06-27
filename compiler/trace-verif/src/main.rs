#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused_variables)]
#![allow(unused_parens)]
#![allow(unused_must_use)]

extern crate common;

use im_rc::hashmap::HashMap;
pub mod typed_traces;

use common::types::{
    Env,
    AExp,
    AExp::{
        Bool,
        Int,
        Id,
        From,
    },
    BExp,
    CExp,
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

fn check_aexp(env: &Env, aexp: &AExp) -> Type {
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

fn check_let(env: &Env, x: String, bexp: &BExp, cexp: &CExp) -> Type {
    let t1 = check_bexp(env, bexp);
    let mut env2 = env.clone();
    env2.insert(x, t1);
    return check_cexp(&env2, cexp);
}

fn check_bexp(env: &Env, bexp: &BExp) -> Type {
    match bexp {
        BExp::AExp(aexp) => return check_aexp(env, aexp),
        BExp::BinOp(aexp1, aexp2) => {
            let t1 = check_aexp(env, aexp1);
            let t2 = check_aexp(env, aexp2);
            if (t1 == t2) {
                return t1;
            } else {
                panic!("Mismatched BinOp types!");
            }
        },
        BExp::Clos(cexps) => {
            let types: Env = cexps.iter()
                .map(|(id, cexp)| (id.to_string(), check_cexp(env, cexp)))
                .collect();
            return TClos(types);
        },
        BExp::Let(x, bexp, cexp) => return check_let(env, x.to_string(), bexp, cexp),
    }
}

fn check_cexp(env: &Env, cexp: &CExp) -> Type {
    match cexp {
        CExp::AExp(aexp) => return check_aexp(env, aexp),
        CExp::Seq(cexp1, cexp2) => {
            check_cexp(env, cexp1);
            return check_cexp(env, cexp2);
        },
        CExp::Let(x, bexp, cexp) => return check_let(env, x.to_string(), bexp, cexp),
        CExp::Set(x, aexp) => {
            let t = check_aexp(env, aexp);
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
        CExp::If(aexp, cexp1, cexp2) => {
            expect_bool(check_aexp(env, aexp));
            let t1 = check_cexp(env, cexp1);
            let t2 = check_cexp(env, cexp2);
            if (t1 == t2) {
                return t1;
            } else {
                panic!("Mismatched If types!");
            }
        },
        CExp::While(aexp, cexp) => {
            expect_bool(check_aexp(env, aexp));
            return check_cexp(env, cexp);
        },
        CExp::Label(_, cexp) => return check_cexp(env, cexp),
        CExp::Break(_, aexp) => return check_aexp(env, aexp),
        CExp::Unknown => return TUnknown,
    }
}

fn main() {
    let test_bexp = BExp::BinOp(Int(10), Int(1));
    println!("{:?}", test_bexp);

    let env: Env = HashMap::new();

    let result = check_bexp(&env, &test_bexp);
    println!("{:?}", result);

}

#[test]
fn test_add() {
    let test_bexp = BExp::BinOp(Int(10), Int(1));
    let env: Env = HashMap::new();
    let result = check_bexp(&env, &test_bexp);
    assert_eq!(result, TInt);
}

#[test]
#[should_panic]
fn test_add_p() {
    let test_bexp = BExp::BinOp(Int(10), Bool(false));
    let env: Env = HashMap::new();
    let result = check_bexp(&env, &test_bexp);
    assert_eq!(result, TInt);
}

#[test]
fn test_let() {
    let test_cexp = {
        CExp::Let("x".to_string(),
            Box::new(BExp::AExp(Int(13))),
            Box::new(CExp::AExp(Id("x".to_string()))))
    };
    let env: Env = HashMap::new();
    let result = check_cexp(&env, &test_cexp);
    assert_eq!(result, TInt);
}

#[test]
fn test_set() {
    let test_cexp = {
        CExp::Let("x".to_string(),
            Box::new(BExp::AExp(Bool(false))),
            Box::new(CExp::Set("x".to_string(),
                Bool(true))))
    };
    let env: Env = HashMap::new();
    let result = check_cexp(&env, &test_cexp);
    assert_eq!(result, TBool);
}

#[test]
#[should_panic]
fn test_set_p() {
    let test_cexp = {
        CExp::Let("x".to_string(),
            Box::new(BExp::AExp(Int(13))),
            Box::new(CExp::Set("x".to_string(),
                Bool(false))))
    };
    let env: Env = HashMap::new();
    let result = check_cexp(&env, &test_cexp);
    assert_eq!(result, TInt);
}

#[test]
fn test_if() {
    let test_cexp = {
        CExp::If(Bool(true),
            Box::new(CExp::AExp(Int(4))),
            Box::new(CExp::AExp(Int(10))))
    };
    let env: Env = HashMap::new();
    let result = check_cexp(&env, &test_cexp);
    assert_eq!(result, TInt);
}

#[test]
#[should_panic]
fn test_if_p() {
    let test_cexp = {
        CExp::If(Bool(true),
            Box::new(CExp::AExp(Int(4))),
            Box::new(CExp::AExp(Bool(false))))
    };
    let env: Env = HashMap::new();
    let result = check_cexp(&env, &test_cexp);
    assert_eq!(result, TInt);
}

#[test]
fn test_while() {
    let test_cexp = {
        CExp::While(Bool(false),
            Box::new(CExp::Seq(Box::new(CExp::AExp(Bool(false))),
                Box::new(CExp::AExp(Bool(false))))))
    };
    let env: Env = HashMap::new();
    let result = check_cexp(&env, &test_cexp);
    assert_eq!(result, TBool);
}

#[test]
#[should_panic]
fn test_while_p() {
    let test_cexp = {
        CExp::While(Int(4),
            Box::new(CExp::Seq(Box::new(CExp::AExp(Bool(false))),
                Box::new(CExp::AExp(Bool(false))))))
    };
    let env: Env = HashMap::new();
    let result = check_cexp(&env, &test_cexp);
    assert_eq!(result, TBool);
}

#[test]
fn test_label() {
    let test_cexp = {
        CExp::Let("computer".to_string(),
            Box::new(BExp::AExp(Int(32))),
            Box::new(CExp::Label("fitbit".to_string(),
                Box::new(CExp::Break("fitbit".to_string(),
                    Id("computer".to_string()))))))
    };
    let env: Env = HashMap::new();
    let result = check_cexp(&env, &test_cexp);
    assert_eq!(result, TInt);
}