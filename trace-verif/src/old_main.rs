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
        SetFrom,
        If,
        While,
        Label,
        Break,
        Unknown
    },
    Type,
    Type::{
        TBool,
        TInt,
        TUnknown,
        TClos,
        TUnit
    },
    State
};

fn expect_bool(ty: Type) {
    match ty {
        TBool => {},
        _ => panic!("Expected TBool."),
    }
}

/*

    Panics if variable names are non-unique.
    i.e. a name can only appear in a `Let` once.

    Re-using State type for this.

*/
#[allow(unused)]
fn check_unique_names(names: &mut State, exp: &Exp) {
    match exp {
        Clos(env2) => {
            env2.iter()
                .map(|(_, e)| check_unique_names(names, e));
        },
        BinOp(exp1, exp2) => {
            check_unique_names(names, exp1);
            check_unique_names(names, exp2);
        },
        Seq(exp1, exp2) => {
            check_unique_names(names, exp1);
            check_unique_names(names, exp2);
        },
        Let(x, exp1, exp2) => {
            check_unique_names(names, exp1);
            match names.get(x) {
                Some(_) => panic!("Found non-unique name."),
                None => {
                    names.insert(x.to_string(), TUnit);
                    check_unique_names(names, exp2);
                }
            };
        },
        Set(_, exp) => check_unique_names(names, exp),
        SetFrom(_, exp) => check_unique_names(names, exp),
        If(exp1, exp2, exp3) => {
            check_unique_names(names, exp1);
            check_unique_names(names, exp2);
            check_unique_names(names, exp3);
        },
        While(exp1, exp2) => {
            check_unique_names(names, exp1);
            check_unique_names(names, exp2);
        },
        Label(_, exp) => check_unique_names(names, exp),
        Break(_, exp) => check_unique_names(names, exp),
        _ => {},
    }
}

fn get_elem_from_tclos(env: &Env, x: &String, y: &String) -> Type {
    match env.get(x) {
        Some(clos) => {
            match clos {
                TClos(env2) => {
                    match env2.get(y) {
                        Some(ty) => return ty.clone(),
                        None => panic!("Id not found."),
                    }
                },
                _ => panic!("Expected closure."),
            }
        },
        None => panic!("Id not found."),
    }
}

/*

    Checks types.
    Fills in state object.

*/
pub fn check_types(state: &mut State, env: &Env, exp: &Exp) -> Type {
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
            let t1 = check_types(state, env, exp1);
            let t2 = check_types(state, env, exp2);
            if t1 == t2 {
                return t1;
            } else {
                panic!("Mismatched BinOp types!");
            }
        },
        Clos(exps) => {
            let types: Env = exps.iter()
                .map(|(id, exp)| (id.to_string(), check_types(state, env, exp)))
                .collect();
            return TClos(types);
        },
        Seq(exp1, exp2) => {
            check_types(state, env, exp1);
            return check_types(state, env, exp2);
        },
        Let(x, exp1, exp2) => {
            let t1 = check_types(state, env, exp1);
            let mut env2 = env.clone();
            env2.insert(x.to_string(), t1.clone());
            state.insert(x.to_string(), t1);
            return check_types(state, &env2, exp2);
        }
        Set(x, exp) => {
            let t = check_types(state, env, exp);
            match env.get(x) {
                Some(other) => {
                    if t == *other {
                        return t;
                    } else {
                        panic!("Mismatched types!");
                    }
                },
                None => panic!("Id not found."),
            }
        },
        SetFrom(names, exp) => {
            let t = check_types(state, env, exp);
            let mut rover = TUnknown;
            let mut x = names.first().unwrap();
            names.iter()
                .next()
                .map(|y| {
                    rover = get_elem_from_tclos(env, x, y);
                    x = y;
                    y
                });
            if(t == rover) {
                return t;
            } else {
                panic!("Mismatched types!");
            }
        }
        If(exp1, exp2, exp3) => {
            expect_bool(check_types(state, env, exp1));
            let t1 = check_types(state, env, exp2);
            let t2 = check_types(state, env, exp3);
            if (t1 == t2) {
                return t1;
            } else {
                panic!("Mismatched If types!");
            }
        },
        While(exp1, exp2) => {
            expect_bool(check_types(state, env, exp1));
            return check_types(state, env, exp2);
        },
        Label(_, exp) => return check_types(state, env, exp),
        Break(_, exp) => return check_types(state, env, exp),
        Unknown => return TUnknown,
    }
}

/*

    Transforms:

    Let(x, exp1, exp2)
        ->
    Seq(SetFrom(vec![state, x] exp1),
        exp2)

    Set(x, exp)
        ->
    SetFrom(vec![state, x], exp);

    SetFrom(x, y, exp);
        ->
    SetFrom(vec![state, x, y], exp);

*/
pub fn state_transformation(exp: &Exp) -> Exp {
    match exp {
        Bool(b) => return Bool(*b),
        Int(n) => return Int(*n),
        Id(x) => return Id(x.to_string()),
        From(x, y) => return From(x.to_string(), y.to_string()),
        Clos(env2) => {
            let env3 = env2.iter()
                .map(|(x, e)| (x.to_string(), Box::new(state_transformation(e))))
                .collect::<Vec<_>>();
            return Clos(env3);
        }
        BinOp(exp1, exp2) => {
            let e1 = state_transformation(exp1);
            let e2 = state_transformation(exp2);
            return BinOp(Box::new(e1), Box::new(e2));
        },
        Seq(exp1, exp2) => {
            let e1 = state_transformation(exp1);
            let e2 = state_transformation(exp2);
            return BinOp(Box::new(e1), Box::new(e2));
        },
        Let(x, exp1, exp2) => {
            let e1 = state_transformation(exp1);
            let e2 = state_transformation(exp2);
            return Seq(Box::new(SetFrom(vec!["state".to_string(), x.to_string()],
                                        Box::new(e1))),
                       Box::new(e2));
        },
        Set(x, exp) => {
            let e = state_transformation(exp);
            return SetFrom(vec!["state".to_string(), x.to_string()],
                            Box::new(e));
        },
        SetFrom(names, exp) => {
            let e = state_transformation(exp);
            let mut new_names = names.clone();
            new_names.insert(0, "state".to_string());
            return SetFrom(new_names, Box::new(e));
        },
        If(exp1, exp2, exp3) => {
            let e1 = state_transformation(exp1);
            let e2 = state_transformation(exp2);
            let e3 = state_transformation(exp3);
            return If(Box::new(e1),
                      Box::new(e2),
                      Box::new(e3));
        },
        While(exp1, exp2) => {
            let e1 = state_transformation(exp1);
            let e2 = state_transformation(exp2);
            return While(Box::new(e1),
                         Box::new(e2));
        },
        Label(x, exp) => {
            let e = state_transformation(exp);
            return Label(x.to_string(), Box::new(e));
        },
        Break(x, exp) => {
            let e = state_transformation(exp);
            return Break(x.to_string(), Box::new(e));
        }
        Unknown => return Unknown,
    }
}

fn main() {
    
}

fn run_test(exp: Exp, solution: Type) {
    let mut names: State = HashMap::new();
    check_unique_names(&mut names, &exp);

    let mut state: State = HashMap::new();
    let env: Env = HashMap::new();
    let result = check_types(&mut state, &env, &exp);
    //println!("\n{:?}\n", state);
    assert_eq!(result, solution);

    let transformed = state_transformation(&exp);
    //println!("\n{:?}", transformed)
}

fn parse_exp(s: &str) -> Exp {
    serde_json::from_str(&s).expect("Expression (as JSON)")
}

fn run_test_json(s: &str, solution: Type) {
    let exp = parse_exp(s);
    run_test(exp, solution);
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

fn let_(name: &str, named: Exp, body: Exp) -> Exp {
    return Let(name.to_string(), Box::new(named), Box::new(body));
}

fn int(n: i32) -> Exp {
    return Int(n);
}

fn id(x: &str) -> Exp {
    return Id(x.to_string());
}

fn clos<'a>(binds: Vec<(&'a str, Exp)>) -> Exp {
    return Clos(binds.into_iter().map(|(x, e)| (x.to_string(), Box::new(e))).collect())
}

#[test]
fn test_fun() {

    // let a = 1 in 
    // let fun = clos(a -> a, ) in 
    // let d = let $0 = fun in  let b = 5 in  let c = $0.a  +  b in  c in  d

    let exp = let_("a",
        int(1),
        let_("fun",
            clos(vec![ ("a", id("a")) ]),
                let_("d",
                    let_("$0", id("fun"),
                            let_("b", int(5),
                                let_("c",
                                    BinOp( Box::new( From("$0".to_string(), "a".to_string())),
                                            Box::new( Id("b".to_string()))),
                                            id("c")))),
                    id("d"))));

    run_test(exp, TInt);

    /*
    run_test_json(
        r#"
            { "Let" : [ "a",
                       { "Int" : 1 },
                       { "Let" : [ "fun",
                                   { "Clos" : [ ["a", { "Id" : "a" }] ] },
                                   { "Let" : [ "d",
                                               { "Let" : [ "$0"
                                                           { "Id" : "fun" },
                                                           { "Let" : [ "b",
                                                                       { "Int" : 5 },
                                                                       { "Let" : }
                                                                     ]
                                                           }
                                                         ]
                                               },
                                               { "Id" : "d" }                                              
                                             ]
                                   }
                                 ]
                       }
                      ]
            }
        "#,
        TInt
    );
    */
}

#[test]
#[should_panic]
fn test_unique_names() {
    run_test(
        Let("x".to_string(),
            Box::new(Bool(false)),
            Box::new(Let("x".to_string(),
                     Box::new(Bool(true)),
                     Box::new(Int(1))))),
        TBool
    );
}