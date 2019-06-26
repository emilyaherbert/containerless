
use std::collections::HashMap;

pub type Env = HashMap<String, Type>;

#[derive(Debug)]
pub enum TrexpAtom {
    Bool(bool),
    Int(i32),
    Id(String)
}

#[derive(Debug)]
pub enum Trexp {
    AExp(TrexpAtom),
    Seq(Box<Trexp>, Box<Trexp>),
    BinOp(Box<Trexp>, Box<Trexp>),
    From(String, String),
    Let(String, Box<Trexp>, Box<Trexp>),
    Set(String, Box<Trexp>, Box<Trexp>),
    If(Box<Trexp>, Box<Trexp>, Box<Trexp>),
    While(Box<Trexp>, Box<Trexp>),
    Label(String, Box<Trexp>),
    Break(String, Box<Trexp>),
    Unknown,
    Clos(HashMap<String, Box<Trexp>>),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Type<'a> {
    TBool,
    TInt,
    TUnknown,
    TClos(HashMap<String, Box<Type>>),
}