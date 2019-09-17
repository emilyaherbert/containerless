use super::error::{Error, type_error};
use bumpalo::{collections::{Vec, String}, Bump};
use std::cell::{Cell, RefCell};
use std::convert::{From, TryFrom};

/**
 * This is an implementation of "type dynamic" for traces.
 */
#[derive(Debug, Copy, Clone)]
pub enum Dyn<'a> {
    Int(i32),
    Float(f64),
    Bool(bool),
    Str(&'a String<'a>),
    Undefined,
    Ref(&'a Cell<Dyn<'a>>),
    Vec(&'a RefCell<Vec<'a, Dyn<'a>>>),
    Object(&'a RefCell<Vec<'a, (&'a str, Dyn<'a>)>>),
}

pub type DynResult<'a> = Result<Dyn<'a>, Error>;

impl<'a> Dyn<'a> {
    pub fn undef() -> Dyn<'a> {
        Dyn::Undefined
    }

    /** Wraps an integer in type `Dyn`. */
    pub fn int(n: i32) -> Dyn<'a> {
        Dyn::Int(n)
    }

    pub fn str(arena: &'a Bump, s: &str) -> Dyn<'a> {
        Dyn::Str(arena.alloc(String::from_str_in(s, arena)))
    }

    /** Wraps a floating-point number in type `Dyn`. */
    pub fn float(x: f64) -> Dyn<'a> {
        Dyn::Float(x)
    }

    pub fn ref_(arena: &'a Bump, value: Dyn<'a>) -> Dyn<'a> {
        Dyn::Ref(arena.alloc(Cell::new(value)))
    }

    pub fn object(arena: &'a Bump, _fields: std::vec::Vec<(&'a str, Dyn<'a>)>) -> Dyn<'a> {
        Dyn::Object(arena.alloc(RefCell::new(Vec::new_in(arena))))
    }

    pub fn add(&self, other: Dyn<'a>) -> DynResult<'a> {
        match (*self, other) {
            (Dyn::Int(m), Dyn::Int(n)) => Ok(Dyn::Int(m + n)),
            (Dyn::Float(x), Dyn::Int(n)) => Ok(Dyn::Float(x + f64::from(n))),
            (Dyn::Int(n), Dyn::Float(x)) => Ok(Dyn::Float(f64::from(n) + x)),
            (Dyn::Float(x), Dyn::Float(y)) => Ok(Dyn::Float(x + y)),
            _ => type_error("add")
        }
    }

    pub fn strict_eq(&self, other: Dyn<'a>) -> DynResult<'a> {
        match (*self, other) {
            (Dyn::Int(m), Dyn::Int(n)) => Ok(Dyn::Bool(m == n)),
            (Dyn::Str(s1), Dyn::Str(s2)) => Ok(Dyn::Bool(s1 == s2)),
            (Dyn::Str(_), other) => panic!("not working Dyn::Str(_) == {:?}", other),
            (that, other) => {
                println!("Other is {:?}", other);
                println!("self is {:?}", self);
                panic!("not working {:?} == {:?}", self, other)
            },
            // (that, other) => panic!("not working {:?}", other),
                // panic!(format!("Trying {:?} === {:?}", that, other))
        }
    }

    /** Array indexing. */
    pub fn index(&self, index: Dyn<'a>) -> DynResult<'a> {
        match (self, index) {
            (Dyn::Vec(vec_cell), Dyn::Int(index)) => match usize::try_from(index) {
                Err(_) => Ok(Dyn::Undefined),
                Ok(index) => {
                    let vec = vec_cell.borrow();
                    if index >= vec.len() {
                        return Ok(Dyn::Undefined);
                    }
                    return Ok(vec[index]);
                }
            },
            _ => type_error("array indexing")
        }
    }

    /** Allocate a vector. */
    pub fn vec(arena: &'a Bump) -> Dyn<'a> {
        Dyn::Vec(arena.alloc(RefCell::new(Vec::new_in(arena))))
    }

    /** push an element into a vector. */
    pub fn push(self, value: Dyn<'a>) {
        match self {
            Dyn::Vec(vec_cell) => vec_cell.borrow_mut().push(value),
            _ => panic!(""),
        }
    }

    pub fn deref(self) -> Dyn<'a> {
        match self {
            Dyn::Ref(cell) => cell.get(),
            // This should never occur, since we insert refs and derefs in the
            // right places.
            _ => panic!("invoked deref on {:?}", self),
        }
    }
}

impl<'a> From<Dyn<'a>> for bool {
    fn from(value: Dyn<'a>) -> Self {
        match value {
            Dyn::Bool(b) => b,
            Dyn::Int(0) => false,
            Dyn::Int(_) => true,
            Dyn::Undefined => false,
            _ => unimplemented!(),
        }
    }
}

impl<'a> From<()> for Dyn<'a> {
    fn from(_value: ()) -> Self {
        Dyn::Undefined
    }
}
