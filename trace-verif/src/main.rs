#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused_variables)]
#![allow(unused_parens)]
#![allow(unused_must_use)]

mod typed_traces;

use im_rc::hashmap::HashMap;

use typed_traces::{
    Env,
    Exp,
    Exp::{
        Bool,
        Int,
        Id,
        From,
        BinOp,
        Clos,
        Seq,
        Let,
        Set,
        If,
        While,
        Label,
        Break,
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

fn check_let(env: &Env, x: String, exp1: &Exp, exp2: &Exp) -> Type {
    let t1 = check_exp(env, exp1);
    let mut env2 = env.clone();
    env2.insert(x, t1);
    return check_exp(&env2, exp2);
}

fn check_exp(env: &Env, exp: &Exp) -> Type {
    match exp {
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
        BinOp(exp1, exp2) => {
            let t1 = check_exp(env, exp1);
            let t2 = check_exp(env, exp2);
            if (t1 == t2) {
                return t1;
            } else {
                panic!("Mismatched BinOp types!");
            }
        },
        Clos(exps) => {
            let types: Env = exps.iter()
                .map(|(id, exp)| (id.to_string(), check_exp(env, exp)))
                .collect();
            return TClos(types);
        },
        Seq(exp1, exp2) => {
            check_exp(env, exp1);
            return check_exp(env, exp2);
        },
        Let(x, exp1, exp2) => return check_let(env, x.to_string(), exp1, exp2),
        Set(x, exp) => {
            let t = check_exp(env, exp);
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
        If(exp1, exp2, exp3) => {
            expect_bool(check_exp(env, exp1));
            let t1 = check_exp(env, exp2);
            let t2 = check_exp(env, exp3);
            if (t1 == t2) {
                return t1;
            } else {
                panic!("Mismatched If types!");
            }
        },
        While(exp1, exp2) => {
            expect_bool(check_exp(env, exp1));
            return check_exp(env, exp2);
        },
        Label(_, exp) => return check_exp(env, exp),
        Break(_, exp) => return check_exp(env, exp),
        Unknown => return TUnknown,
    }
}

fn main() {
    let test_exp = BinOp(Box::new(Int(10)), Box::new(Int(1)));
    println!("{:?}", test_exp);

    let env: Env = HashMap::new();

    let result = check_exp(&env, &test_exp);
    println!("{:?}", result);

}

#[test]
fn test_add() {
    let test_exp = BinOp(Box::new(Int(10)), Box::new(Int(1)));
    let env: Env = HashMap::new();
    let result = check_exp(&env, &test_exp);
    assert_eq!(result, TInt);
}

#[test]
#[should_panic]
fn test_add_p() {
    let test_exp = BinOp(Box::new(Int(10)), Box::new(Bool(false)));
    let env: Env = HashMap::new();
    let result = check_exp(&env, &test_exp);
    assert_eq!(result, TInt);
}

#[test]
fn test_let() {
    let test_exp = {
        Let("x".to_string(),
            Box::new(Int(13)),
            Box::new(Id("x".to_string())))
    };
    let env: Env = HashMap::new();
    let result = check_exp(&env, &test_exp);
    assert_eq!(result, TInt);
}

#[test]
fn test_set() {
    let test_exp = {
        Let("x".to_string(),
            Box::new(Bool(false)),
            Box::new(Set("x".to_string(),
                Box::new(Bool(true)))))
    };
    let env: Env = HashMap::new();
    let result = check_exp(&env, &test_exp);
    assert_eq!(result, TBool);
}

#[test]
#[should_panic]
fn test_set_p() {
    let test_exp = {
        Let("x".to_string(),
            Box::new(Int(13)),
            Box::new(Set("x".to_string(),
                Box::new(Bool(false)))))
    };
    let env: Env = HashMap::new();
    let result = check_exp(&env, &test_exp);
    assert_eq!(result, TInt);
}

#[test]
fn test_if() {
    let test_exp = {
        If(Box::new(Bool(true)),
            Box::new(Int(4)),
            Box::new(Int(10)))
    };
    let env: Env = HashMap::new();
    let result = check_exp(&env, &test_exp);
    assert_eq!(result, TInt);
}

#[test]
#[should_panic]
fn test_if_p() {
    let test_exp = {
        If(Box::new(Bool(true)),
            Box::new(Int(4)),
            Box::new(Bool(false)))
    };
    let env: Env = HashMap::new();
    let result = check_exp(&env, &test_exp);
    assert_eq!(result, TInt);
}

#[test]
fn test_while() {
    let test_exp = {
        While(Box::new(Bool(false)),
            Box::new(Seq(Box::new(Bool(false)),
                Box::new(Bool(false)))))
    };
    let env: Env = HashMap::new();
    let result = check_exp(&env, &test_exp);
    assert_eq!(result, TBool);
}

#[test]
#[should_panic]
fn test_while_p() {
    let test_exp = {
        While(Box::new(Int(4)),
            Box::new(Seq(Box::new(Bool(false)),
                Box::new(Bool(false)))))
    };
    let env: Env = HashMap::new();
    let result = check_exp(&env, &test_exp);
    assert_eq!(result, TBool);
}

#[test]
fn test_label() {
    let test_exp = {
        Let("computer".to_string(),
            Box::new(Int(32)),
            Box::new(Label("fitbit".to_string(),
                Box::new(Break("fitbit".to_string(),
                    Box::new(Id("computer".to_string())))))))
    };
    let env: Env = HashMap::new();
    let result = check_exp(&env, &test_exp);
    assert_eq!(result, TInt);
}