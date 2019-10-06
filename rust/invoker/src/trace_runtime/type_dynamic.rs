use super::error::{type_error, Error};
use bumpalo::{
    collections::{String, Vec},
    Bump,
};
use std::cell::{Cell, RefCell};
use std::convert::{From, TryFrom};

pub fn unknown<'a>() -> DynResult<'a> {
    Err(Error::Unknown)
}

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

    pub fn object(arena: &'a Bump) -> Dyn<'a> {
        Dyn::Object(arena.alloc(RefCell::new(Vec::new_in(arena))))
    }

    pub fn object_with(arena: &'a Bump, fields: std::vec::Vec<(&'static str, Dyn<'a>)>) -> Dyn<'a> {
        let obj = Dyn::object(arena);
        for (k, v) in fields.into_iter() {
            obj.set_field(k, v).unwrap();
        }
        return obj;
    }

    pub fn set_field(&self, key: &'static str, value: Dyn<'a>) -> DynResult<'a> {
        // This is a pretty bad implementation. We are scanning a vector!
        if let Dyn::Object(cell) = self {
            let mut vec = cell.borrow_mut();
            for (k, v) in vec.iter_mut() {
                if *k == key {
                    *v = value;
                    return Ok(Dyn::Undefined);
                }
            }
            vec.push((key, value));
            return Ok(Dyn::Undefined);
        }
        else {
            return type_error("set_field");
        }
    }

    pub fn get(&self, key: &str) -> DynResult<'a> {
        match self {
            Dyn::Object(cell) => {
                let vec = cell.borrow();
                for (k, v) in vec.iter() {
                    if *k == key {
                        return Ok(*v);
                    }
                }
                return Ok(Dyn::Undefined);
            }
            _ => return type_error("not an object")
        }
    }

    pub fn add(&self, other: Dyn<'a>) -> DynResult<'a> {
        match (*self, other) {
            (Dyn::Int(m), Dyn::Int(n)) => Ok(Dyn::Int(m + n)),
            (Dyn::Float(x), Dyn::Int(n)) => Ok(Dyn::Float(x + f64::from(n))),
            (Dyn::Int(n), Dyn::Float(x)) => Ok(Dyn::Float(f64::from(n) + x)),
            (Dyn::Float(x), Dyn::Float(y)) => Ok(Dyn::Float(x + y)),
            _ => type_error("add"),
        }
    }

    pub fn strict_eq(&self, other: Dyn<'a>) -> DynResult<'a> {
        match (*self, other) {
            (Dyn::Int(m), Dyn::Int(n)) => Ok(Dyn::Bool(m == n)),
            (Dyn::Str(s1), Dyn::Str(s2)) => Ok(Dyn::Bool(s1 == s2)),
            (Dyn::Str(_), other) => panic!("not working Dyn::Str(_) == {:?}", other),
            (_that, other) => {
                println!("Other is {:?}", other);
                println!("self is {:?}", self);
                panic!("not working {:?} == {:?}", self, other)
            }
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
            _ => type_error("array indexing"),
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

impl<'a> TryFrom<Dyn<'a>> for std::string::String {
    type Error = ();

    fn try_from(value: Dyn<'a>) -> Result<Self, ()> {
        match value {
            Dyn::Str(s) => Ok(s.to_string()),
            _ => Err(()),
        }
    }
}
