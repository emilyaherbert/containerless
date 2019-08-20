
use bumpalo::{Bump, collections::Vec};
use std::cell::{RefCell};
use std::convert::TryFrom;
use super::error::Error;

/**
 * This is an implementation of "type dynamic" for traces.
 */
#[derive(Copy, Clone)]
pub enum Dyn<'a> {
    Int(i32),
    Float(f64),
    Bool(bool),
    Str(&'a str),
    Undefined,
    Unknown,
    Vec(&'a RefCell<Vec<'a, Dyn<'a>>>)
    // Object(Vec<String,Dyn>)
}

pub type DynResult<'a> = Result<Dyn<'a>,Error>;

impl<'a> Dyn<'a> {

    /** Wraps an integer in type `Dyn`. */
    pub fn int(n: i32) -> Dyn<'a> {
        Dyn::Int(n)
    }

    /** Wraps a floating-point number in type `Dyn`. */
    pub fn float(x: f64) -> Dyn<'a> {
        Dyn::Float(x)
    }

    pub fn add(&self, other: &Dyn<'a>) -> DynResult<'a> {
        match (self, other) {
            (Dyn::Int(m), Dyn::Int(n)) => Ok(Dyn::Int(m + n)),
            (Dyn::Float(x), Dyn::Int(n)) => Ok(Dyn::Float(*x + f64::from(*n))),
            (Dyn::Int(n), Dyn::Float(x)) => Ok(Dyn::Float(f64::from(*n) + *x)),
            (Dyn::Float(x), Dyn::Float(y)) => Ok(Dyn::Float(x + y)),
            _ => Err(Error::TypeError)
        }
    }

    /** Array indexing. */
    pub fn index(&'a self, index: Dyn<'a>) -> DynResult<'a> {
        match (self, index) {
            (Dyn::Vec(vec_cell), Dyn::Int(index)) => {
                match usize::try_from(index) {
                    Err(_) => Ok(Dyn::Undefined),
                    Ok(index) => {
                        let vec = vec_cell.borrow();
                        if index >= vec.len() {
                            return Ok(Dyn::Undefined);
                        }
                        return Ok(vec[index]);
                    }
                }
            },
            _ => Err(Error::TypeError)
        }
    }

    /** Allocate a vector. */
    pub fn vec(arena: &'a Bump) -> Dyn<'a> {
        Dyn::Vec(arena.alloc(RefCell::new(Vec::new_in(arena))))
    }

    /** push an element into a vector. */
    pub fn push(&'a self, value: Dyn<'a>) {
        match self {
            Dyn::Vec(vec_cell) => {
                vec_cell.borrow_mut().push(value)
            },
            _ => panic!("")
        }
    }
}