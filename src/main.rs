extern crate im;

use im::HashMap;
use std::rc::Rc;
use std::ops::Deref;

type Env = Rc<HashMap<String, Rc<Val>>>;

#[derive(Clone, Debug, PartialEq)] 
enum Expr {
    Bool(bool),
    Id(String),
    App(Box<Expr>, Box<Expr>),
    Fun(String, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>)
}

#[derive(Clone, Debug, PartialEq)] 
enum Val {
    Bool(bool),
    Clos(Env, String, Box<Expr>)
}

fn eval(env : Env, expr: &Expr) -> Rc<Val> {
    match expr {
        Expr::Bool(b) => Rc::new(Val::Bool(*b)),
        Expr::Id(x) => env.get(x).unwrap().clone(),
        Expr::App(e1, e2) => {
            let env_copy = env.clone();
            match eval(env, &*e1).deref() {
                Val::Clos(env2, x, body) => {
                    let v2 = eval(env_copy, &*e2);
                    let env2_copy = env2.clone();
                    eval(Rc::new(env2_copy.update(x.to_string(), v2)), &*body)
                },
                _ => panic!("no closure")
            }
        },
        Expr::Fun(x, body) => {
            let x_copy = x.clone();
            let body_copy = body.clone();
            Rc::new(Val::Clos(env, x_copy, body_copy))
        },
        Expr::If(e1, e2, e3) => {
            let env_copy = env.clone();
            match *eval(env, &*e1) {
                Val::Bool(b) => eval(env_copy, if b { &*e2 } else { &*e3 }),
                _ => panic!("no bool")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use im::HashMap;
    use std::rc::Rc;

    #[test]
    fn eval_bool() {
        // evaluate a boolean value (true) to itself
        let env: Env = Rc::new(HashMap::new());
        let exp: Expr = Expr::Bool(true);
        let val: Rc<Val> = eval(env.clone(), &exp);
        println!("{:?} evaluates to {:?}", exp, val);
        assert_eq!(val, Rc::new(Val::Bool(true)));
    }

    #[test]
    fn eval_if() {
        // evaluate an if expression which negates the value of x in the env
        let mut env_0: HashMap<String, Rc<Val>> = HashMap::new();
        env_0.insert(String::from("x"),
                     Rc::new(Val::Bool(false)));
        let env: Env = Rc::new(env_0);
        let exp: Expr = Expr::If(Box::new(Expr::Id(String::from("x"))),
                                 Box::new(Expr::Bool(false)),
                                 Box::new(Expr::Bool(true)));
        let val: Rc<Val> = eval(env.clone(), &exp);
        println!("{:?} evaluates to {:?}", exp, val);
        assert_eq!(val, Rc::new(Val::Bool(true)));
    }

    #[test]
    fn eval_apply() {
        // apply a function which negates its argument, to the value "false"
        let env: Env = Rc::new(HashMap::new());
        let exp: Expr = Expr::App(Box::new(Expr::Fun(String::from("x"),
                Box::new(Expr::If(Box::new(Expr::Id(String::from("x"))),
                                  Box::new(Expr::Bool(false)),
                                  Box::new(Expr::Bool(true)))))),
            Box::new(Expr::Bool(false)));
        let val: Rc<Val> = eval(env.clone(), &exp);
        println!("{:?} evaluates to {:?}", exp, val);
        assert_eq!(val, Rc::new(Val::Bool(true)));
    }
}

fn main() {
    println!("Hello, world!");
}
