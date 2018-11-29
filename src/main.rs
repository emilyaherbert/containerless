extern crate im;

use im::HashMap;
use std::rc::Rc;

type Env = Rc<HashMap<String, Rc<Val>>>;

#[derive(Clone)] 
enum Expr {
    Bool(bool),
    Id(String),
    App(Box<Expr>, Box<Expr>),
    Fun(String, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>)
}

#[derive(Clone)] 
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
            match *eval(env, &*e1) {
                Val::Clos(env2, x, body) => {
                    let v2 = eval(env_copy, &*e2);
                    let env2_copy = env2.clone();
                    eval(Rc::new(env2_copy.update(x, v2)), &*body)
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

fn main() {
    println!("Hello, world!");
}
