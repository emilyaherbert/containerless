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
    },
    State
};

fn expect_bool(ty: Type) {
    match ty {
        TBool => {},
        _ => panic!("Expected TBool."),
    }
}

fn check_exp(state: &mut State, env: &Env, exp: &Exp) -> Type {
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
            let t1 = check_exp(state, env, exp1);
            let t2 = check_exp(state, env, exp2);
            if (t1 == t2) {
                return t1;
            } else {
                panic!("Mismatched BinOp types!");
            }
        },
        Clos(exps) => {
            let types: Env = exps.iter()
                .map(|(id, exp)| (id.to_string(), check_exp(state, env, exp)))
                .collect();
            return TClos(types);
        },
        Seq(exp1, exp2) => {
            check_exp(state, env, exp1);
            return check_exp(state, env, exp2);
        },
        Let(x, exp1, exp2) => {
            let t1 = check_exp(state, env, exp1);
            let mut env2 = env.clone();
            env2.insert(x.to_string(), t1.clone());
            state.insert(x.to_string(), t1);
            return check_exp(state, &env2, exp2);
        }
        Set(x, exp) => {
            let t = check_exp(state, env, exp);
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
            expect_bool(check_exp(state, env, exp1));
            let t1 = check_exp(state, env, exp2);
            let t2 = check_exp(state, env, exp3);
            if (t1 == t2) {
                return t1;
            } else {
                panic!("Mismatched If types!");
            }
        },
        While(exp1, exp2) => {
            expect_bool(check_exp(state, env, exp1));
            return check_exp(state, env, exp2);
        },
        Label(_, exp) => return check_exp(state, env, exp),
        Break(_, exp) => return check_exp(state, env, exp),
        Unknown => return TUnknown,
    }
}

fn main() {
    let test_exp = BinOp(Box::new(Int(10)), Box::new(Int(1)));
    println!("{:?}", serde_json::to_string(&test_exp));

    println!("{:?}", test_exp);

    let mut state: State = HashMap::new();
    let env: Env = HashMap::new();

    let result = check_exp(&mut state, &env, &test_exp);
    println!("{:?}", result);

}

fn run_test(exp: Exp, solution: Type) {
    let mut state: State = HashMap::new();
    let env: Env = HashMap::new();
    let result = check_exp(&mut state, &env, &exp);
    println!("\n{:?}\n", state);
    assert_eq!(result, solution);
}

fn parse_exp(s: &str) -> Exp {
    serde_json::from_str(&s).expect("Expression (as JSON)")
}

fn run_test_json(s: &str, solution: Type) {
    let exp = parse_exp(s);
    let mut state: State = HashMap::new();
    let env: Env = HashMap::new();
    let result = check_exp(&mut state, &env, &exp);
    println!("\n{:?}\n", state);
    assert_eq!(result, solution);
}

#[test]
fn parse_add() {
    assert_eq!(parse_exp(r#"
        { "BinOp": [{ "Int" : 10 },
                    { "Int" : 1  }]}
    "#), BinOp(Box::new(Int(10)), Box::new(Int(1))));
}

#[test]
fn test_add() {
    run_test(
        BinOp(Box::new(Int(10)), Box::new(Int(1))),
        TInt
    );
}

#[test]
#[should_panic]
fn test_add_p() {
    run_test(
        BinOp(Box::new(Int(10)), Box::new(Bool(false))),
        TInt
    );
}

#[test]
fn test_let() {
    run_test(
        Let("x".to_string(),
            Box::new(Int(13)),
            Box::new(Id("x".to_string()))),
        TInt
    );
}

#[test]
fn test_set() {
    run_test(
        Let("x".to_string(),
            Box::new(Bool(false)),
            Box::new(Set("x".to_string(),
                Box::new(Bool(true))))),
        TBool
    );
}

#[test]
#[should_panic]
fn test_set_p() {
    run_test(
        Let("x".to_string(),
            Box::new(Int(13)),
            Box::new(Set("x".to_string(),
                Box::new(Bool(false))))),
        TInt
    );
}

#[test]
fn test_if() {
    run_test(
        If(Box::new(Bool(true)),
            Box::new(Int(4)),
            Box::new(Int(10))),
        TInt
    );
}

#[test]
#[should_panic]
fn test_if_p() {
    run_test(
        If(Box::new(Bool(true)),
            Box::new(Int(4)),
            Box::new(Bool(false))),
        TInt
    );
}

#[test]
fn test_while() {
    run_test(
        While(Box::new(Bool(false)),
            Box::new(Seq(Box::new(Bool(false)),
                Box::new(Bool(false))))),
        TBool
    );
}

#[test]
#[should_panic]
fn test_while_p() {
    run_test(
        While(Box::new(Int(4)),
            Box::new(Seq(Box::new(Bool(false)),
                Box::new(Bool(false))))),
        TBool
    );
}

#[test]
fn test_label() {
    run_test(
        Let("computer".to_string(),
            Box::new(Int(32)),
            Box::new(Label("fitbit".to_string(),
                Box::new(Break("fitbit".to_string(),
                    Box::new(Id("computer".to_string()))))))),
        TInt
    );
}

#[test]
fn test_if_json() {
    run_test_json(
        r#"
            { "If" : [{ "Bool" : true },
                      { "Int" : 7 },
                      { "Int" : 9 }]}
        "#,
        TInt
    );
}