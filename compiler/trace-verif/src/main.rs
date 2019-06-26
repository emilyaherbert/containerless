#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused_variables)]
#![allow(unused_parens)]
#![allow(unused_must_use)]

extern crate common;

use im_rc::hashmap::HashMap;

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

fn check_aexp(env: &Env, aexp: &TrexpAtom) -> Type {
    match aexp {
        Bool(_) => return TBool,
        Int(_) => return TInt,
        Id(x) => {
            let ty = env.get(x)
                .unwrap_or_else(|| panic!("Id not found."));
            return ty.clone();
        },
    }
}

fn check_trexp(env: &Env, trexp: &Trexp) -> Type {
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
        Let(x, trexp1, trexp2) => {
            let t1 = check_trexp(env, trexp1);
            let mut env2 = env.clone();
            env2.insert(x.to_string(), t1);
            return check_trexp(&env2, trexp2);
        },
        Set(x, trexp) => {
            let t = check_trexp(env, trexp);
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
            let types: Env = trexps.iter()
                .map(|(id, trexp)| (id.to_string(), check_trexp(env, trexp)))
                .collect();
            return TClos(types);
        }
    }
}

fn main() {
    let test_trexp = BinOp(Box::new(AExp(Int(10))), Box::new(AExp(Int(1))));
    println!("{:?}", test_trexp);

    let env: Env = HashMap::new();

    let result = check_trexp(&env, &test_trexp);
    println!("{:?}", result);

}

#[test]
fn test_add() {
    let test_trexp = BinOp(Box::new(AExp(Int(10))), Box::new(AExp(Int(1))));
    let env: Env = HashMap::new();
    let result = check_trexp(&env, &test_trexp);
    assert_eq!(result, TInt);
}

#[test]
#[should_panic]
fn test_add_p() {
    let test_trexp = BinOp(Box::new(AExp(Int(10))), Box::new(AExp(Bool(false))));
    let env: Env = HashMap::new();
    let result = check_trexp(&env, &test_trexp);
    assert_eq!(result, TInt);
}

#[test]
fn test_let() {
    let test_trexp = {
        Let("x".to_string(),
            Box::new(AExp(Int(13))),
            Box::new(AExp(Id("x".to_string()))))
    };
    let env: Env = HashMap::new();
    let result = check_trexp(&env, &test_trexp);
    assert_eq!(result, TInt);
}

#[test]
fn test_set() {
    let test_trexp = {
        Let("x".to_string(),
            Box::new(AExp(Bool(false))),
            Box::new(Set("x".to_string(),
                Box::new(AExp(Bool(true))))))
    };
    let env: Env = HashMap::new();
    let result = check_trexp(&env, &test_trexp);
    assert_eq!(result, TBool);
}

#[test]
#[should_panic]
fn test_set_p() {
    let test_trexp = {
        Let("x".to_string(),
            Box::new(AExp(Int(13))),
            Box::new(Set("x".to_string(),
                Box::new(AExp(Bool(false))))))
    };
    let env: Env = HashMap::new();
    let result = check_trexp(&env, &test_trexp);
    assert_eq!(result, TInt);
}

#[test]
fn test_if() {
    let test_trexp = {
        If(Box::new(AExp(Bool(true))),
            Box::new(AExp(Int(4))),
            Box::new(AExp(Int(10))))
    };
    let env: Env = HashMap::new();
    let result = check_trexp(&env, &test_trexp);
    assert_eq!(result, TInt);
}

#[test]
#[should_panic]
fn test_if_p() {
    let test_trexp = {
        If(Box::new(AExp(Bool(true))),
            Box::new(AExp(Int(4))),
            Box::new(AExp(Bool(false))))
    };
    let env: Env = HashMap::new();
    let result = check_trexp(&env, &test_trexp);
    assert_eq!(result, TInt);
}

#[test]
fn test_while() {
    let test_trexp = {
        While(Box::new(AExp(Bool(false))),
            Box::new(Seq(Box::new(BinOp(Box::new(AExp(Int(10))), Box::new(AExp(Int(1))))),
                Box::new(AExp(Bool(false))))))
    };
    let env: Env = HashMap::new();
    let result = check_trexp(&env, &test_trexp);
    assert_eq!(result, TBool);
}

#[test]
#[should_panic]
fn test_while_p() {
    let test_trexp = {
        While(Box::new(AExp(Int(4))),
            Box::new(Seq(Box::new(BinOp(Box::new(AExp(Int(10))), Box::new(AExp(Int(1))))),
                Box::new(AExp(Bool(false))))))
    };
    let env: Env = HashMap::new();
    let result = check_trexp(&env, &test_trexp);
    assert_eq!(result, TBool);
}

#[test]
fn test_label() {
    let test_trexp = {
        Label("fitbit".to_string(),
            Box::new(Break("fitbit".to_string(),
                Box::new(Unknown))))
    };
    let env: Env = HashMap::new();
    let result = check_trexp(&env, &test_trexp);
    assert_eq!(result, TUnknown);
}