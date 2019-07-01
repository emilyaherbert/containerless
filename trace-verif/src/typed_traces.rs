
use im_rc::hashmap::HashMap;

use serde::{
    Deserialize,
    Serialize,
};

pub type Env = HashMap<String, Type>;

/*
 * Example 1A:
 *
 * let x = 200 in
 * let y = x + 10 in
 * x
 *
 *    compiles to:
 *
 *  struct State<'a> { x: &'a mut i32, y: &'a mut i32 }
 *
 *  struct MyFunction { }
 *
 *  impl Decontainerized for MyFunction {
 * 
 *  .... lots of garbage ...
 *
 *    fn callback<'a>(arena: &'a Bump, state: &'a mut State) {
 *      state.x = xarena.alloc(200);
 *      state.y = *state.x + 10;
 *      // do not return x
 *    }
 * 
 * }
 *
 *
 * Example 2:
 *
 * let x = 200 in
 * let cl = clos(x -> x) in
 * cl.x = 10;
 * x
 *
 * struct Closure1<'a> {
 *   x: &'a mut i32
 * }
 *
 * let mut x = 200;
 * let cl = Closure1{ x: &mut x };
 * cl.x = 10;
 * return x;
 */

// Constants
#[derive(Debug, Deserialize, Serialize, PartialEq)]
pub enum Exp {
    Bool(bool),
    Int(i32),
    Id(String),
    From(String, String),
    Clos(Vec<(String, Box<Exp>)>),
    BinOp(Box<Exp>, Box<Exp>),
    Seq(Box<Exp>, Box<Exp>),
    Let(String, Box<Exp>, Box<Exp>),
    Set(String, Box<Exp>),
    SetFrom(Vec<String>, Box<Exp>),
    If(Box<Exp>, Box<Exp>, Box<Exp>),
    While(Box<Exp>, Box<Exp>),
    Label(String, Box<Exp>),
    Break(String, Box<Exp>),
    Unknown,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    TBool,
    TInt,
    TUnknown,
    TUnit,
    TClos(Env),
}

pub type State = HashMap<String, Type>;