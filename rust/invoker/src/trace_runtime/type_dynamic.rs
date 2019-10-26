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

#[derive(Debug, Copy, Clone)]
pub struct DynObject<'a> {
  fields: &'a RefCell<Vec<'a, (&'a str, Dyn<'a>)>>
}

impl<'a> DynObject<'a> {

    pub fn new(arena: &'a Bump) -> DynObject<'a> {
        DynObject { fields: arena.alloc(RefCell::new(Vec::new_in(arena))) }
    }

    pub fn from(arena: &'a Bump, fields: std::vec::Vec<(&'static str, Dyn<'a>)>) -> DynObject<'a> {
        let obj = Self::new(arena);
        for (k, v) in fields.into_iter() {
            // TODO(arjun): very naive
            obj.set(k, v);
        }
        return obj;
    }

    pub fn set(&self, key: &'static str, value: Dyn<'a>) {
        // This is a pretty bad implementation. We are scanning a vector!
        let mut vec = self.fields.borrow_mut();
        for (k, v) in vec.iter_mut() {
            if *k == key {
                *v = value;
                return;
            }
        }
        vec.push((key, value));
    }

    pub fn get(&self, key: &str) -> Dyn<'a> {
        let vec = self.fields.borrow();
        for (k, v) in vec.iter() {
            if *k == key {
                return *v;
            }
        }
        return Dyn::Undefined;
    }

    pub fn to_json(&self) -> serde_json::Value {
        use serde_json::Map;
        let mut map = Map::new();
        for (k, v) in self.fields.borrow().iter() {
            if let Some(v_json) = v.to_json() {
                map.insert(k.to_string(), v_json);
            }
        }
        return serde_json::Value::Object(map);
    }
}

#[derive(Debug, Copy, Clone)]
pub struct DynVec<'a> {
  elems: &'a RefCell<Vec<'a, Dyn<'a>>>
}

impl<'a> DynVec<'a> {

    pub fn new(arena: &'a Bump) -> DynVec<'a> {
        DynVec { elems: arena.alloc(RefCell::new(Vec::new_in(arena))) }
    }

    pub fn from(arena: &'a Bump, elems: std::vec::Vec<Dyn<'a>>) -> DynVec<'a> {
        let v = arena.alloc(RefCell::new(Vec::new_in(arena)));
        for e in elems.into_iter() {
            v.borrow_mut().push(e);   
        }
        DynVec { elems: v }
    }

    pub fn get(&self, prop: &str) -> Dyn<'a> {
        match prop {
            "length" => Dyn::float(self.elems.borrow().len() as f64),
            p => unimplemented!("{:?}", p)
        }
    }

    pub fn index(&self, index: i32) -> Dyn<'a> {
        match usize::try_from(index) {
            Err(_) => Dyn::Undefined,
            Ok(index) => {
                let vec = self.elems.borrow();
                if index >= vec.len() {
                    return Dyn::Undefined;
                }
                return vec[index];
            }
        }
    }

    pub fn push(self, value: Dyn<'a>) {
        self.elems.borrow_mut().push(value);
    }

    pub fn to_json(&self) -> serde_json::Value {
        use serde_json::Value;
        Value::Array(self.elems.borrow().iter().filter_map(|x| x.to_json()).collect())
    }
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
    Vec(DynVec<'a>),
    Object(DynObject<'a>),
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

    pub fn bool(b: bool) -> Dyn<'a> {
        Dyn::Bool(b)
    }

    pub fn ref_(arena: &'a Bump, value: Dyn<'a>) -> Dyn<'a> {
        Dyn::Ref(arena.alloc(Cell::new(value)))
    }

    pub fn setref(&self, new_value: Dyn<'a>) -> DynResult<'a> {
        if let Dyn::Ref(cell) = self {
            cell.set(new_value);
            return Ok(Dyn::Undefined);
        }
        panic!("setref on {:?}", self);
    }

    pub fn object(arena: &'a Bump) -> Dyn<'a> {
        Dyn::Object(DynObject::new(arena))
    }

    pub fn object_with(arena: &'a Bump, fields: std::vec::Vec<(&'static str, Dyn<'a>)>) -> Dyn<'a> {
        Dyn::Object(DynObject::from(arena, fields))
    }

    pub fn set_field(&self, key: &'static str, value: Dyn<'a>) -> DynResult<'a> {
        // This is a pretty bad implementation. We are scanning a vector!
        if let Dyn::Object(o) = self {
            o.set(key, value);
            return Ok(Dyn::Undefined);
        } else {
            return type_error("set_field");
        }
    }

    pub fn get(&self, key: &str) -> DynResult<'a> {
        match self {
            Dyn::Object(o) => Ok(o.get(key)),
            Dyn::Vec(v) => Ok(v.get(key)),
            Dyn::Str(s) => {
                match key {
                    "length" => Ok(Dyn::int(s.len() as i32)),
                    _ => unimplemented!()
                }
            }
            _ => type_error("not an object"),
        }
    }

    pub fn add(&self, other: Dyn<'a>) -> DynResult<'a> {
        match (*self, other) {
            (Dyn::Int(m), Dyn::Int(n)) => Ok(Dyn::Int(m + n)),
            (Dyn::Float(x), Dyn::Int(n)) => Ok(Dyn::Float(x + f64::from(n))),
            (Dyn::Int(n), Dyn::Float(x)) => Ok(Dyn::Float(f64::from(n) + x)),
            (Dyn::Float(x), Dyn::Float(y)) => Ok(Dyn::Float(x + y)),
            _ => type_error(&format!("add({:?}, {:?})", &self, &other)),
        }
    }

    pub fn sub(&self, other: Dyn<'a>) -> DynResult<'a> {
        match (*self, other) {
            (Dyn::Int(m), Dyn::Int(n)) => Ok(Dyn::Int(m - n)),
            (Dyn::Float(x), Dyn::Int(n)) => Ok(Dyn::Float(x - f64::from(n))),
            (Dyn::Int(n), Dyn::Float(x)) => Ok(Dyn::Float(f64::from(n) - x)),
            (Dyn::Float(x), Dyn::Float(y)) => Ok(Dyn::Float(x - y)),
            _ => type_error(&format!("add({:?}, {:?})", &self, &other)),
        }
    }

    pub fn strict_neq(&self, other: Dyn<'a>) -> DynResult<'a> {
        self.strict_eq(other).map(|r| match r {
            Dyn::Bool(x) => Dyn::Bool(!x),
            r => panic!(".strict_eq produced {:?}", r)
        })
    }

    pub fn strict_eq(&self, other: Dyn<'a>) -> DynResult<'a> {
        match (*self, other) {
            (Dyn::Undefined, Dyn::Undefined) => Ok(Dyn::Bool(true)),
            (Dyn::Undefined, _) => Ok(Dyn::Bool(false)),
            (_, Dyn::Undefined) => Ok(Dyn::Bool(false)),
            (Dyn::Int(m), Dyn::Int(n)) => Ok(Dyn::Bool(m == n)),
            (Dyn::Str(s1), Dyn::Str(s2)) => Ok(Dyn::Bool(s1 == s2)),
            (Dyn::Str(_), other) => panic!("not working Dyn::Str(_) == {:?}", other),
            (Dyn::Float(x), Dyn::Float(y)) => Ok(Dyn::Bool(x == y)),
            (_that, other) => {
                println!("Other is {:?}", other);
                println!("self is {:?}", self);
                panic!("not working {:?} == {:?}", self, other)
            } // (that, other) => panic!("not working {:?}", other),
              // panic!(format!("Trying {:?} === {:?}", that, other))
        }
    }

    pub fn gt(&self, other: Dyn<'a>) -> DynResult<'a> {
        match (*self, other) {
            (Dyn::Int(m), Dyn::Int(n)) => Ok(Dyn::Bool(m > n)),
            (Dyn::Float(x), Dyn::Float(y)) => Ok(Dyn::Bool(x > y)),
            _ => type_error("gt"),
        }
    }

    pub fn lt(&self, other: Dyn<'a>) -> DynResult<'a> {
        match (*self, other) {
            (Dyn::Int(m), Dyn::Int(n)) => Ok(Dyn::Bool(m < n)),
            (Dyn::Float(x), Dyn::Float(y)) => Ok(Dyn::Bool(x < y)),
            _ => type_error("lt"),
        }
    }

    pub fn gte(&self, other: Dyn<'a>) -> DynResult<'a> {
        match (*self, other) {
            (Dyn::Int(m), Dyn::Int(n)) => Ok(Dyn::Bool(m >= n)),
            (Dyn::Float(x), Dyn::Float(y)) => Ok(Dyn::Bool(x >= y)),
            (Dyn::Int(m), Dyn::Float(n)) => Ok(Dyn::Bool((m as f64) >= n)),
            (Dyn::Float(m), Dyn::Int(n)) => Ok(Dyn::Bool(m >= (n as f64))),
            _ => type_error("gte"),
        }
    }

    pub fn lte(&self, other: Dyn<'a>) -> DynResult<'a> {
        match (*self, other) {
            (Dyn::Int(m), Dyn::Int(n)) => Ok(Dyn::Bool(m <= n)),
            (Dyn::Float(x), Dyn::Float(y)) => Ok(Dyn::Bool(x <= y)),
            (Dyn::Int(m), Dyn::Float(n)) => Ok(Dyn::Bool((m as f64) <= n)),
            (Dyn::Float(m), Dyn::Int(n)) => Ok(Dyn::Bool(m <= (n as f64))),
            _ => type_error("lte"),
        }
    }

    pub fn and(&self, other: Dyn<'a>) -> DynResult<'a> {
        match (*self, other) {
            (Dyn::Bool(x), Dyn::Bool(y)) => Ok(Dyn::Bool(x && y)),
            _ => type_error("and"),
        }
    }

    pub fn or(&self, other: Dyn<'a>) -> DynResult<'a> {
        match (*self, other) {
            (Dyn::Bool(x), Dyn::Bool(y)) => Ok(Dyn::Bool(x || y)),
            _ => type_error("or"),
        }
    }

    /** Array indexing. */
    pub fn index(&self, index: Dyn<'a>) -> DynResult<'a> {
        match (self, index) {
            (Dyn::Vec(vec_cell), Dyn::Int(index)) => Ok(vec_cell.index(index)),
            (Dyn::Vec(vec_cell), Dyn::Float(index)) => Ok(vec_cell.index(index as i32)),
            _ => type_error("array indexing"),
        }
    }

    /** Allocate a vector. */
    pub fn vec(arena: &'a Bump) -> Dyn<'a> {
        Dyn::Vec(DynVec::new(arena))
    }

    pub fn vec_with(arena: &'a Bump, elems: std::vec::Vec<Dyn<'a>>) -> Dyn<'a> {
        Dyn::Vec(DynVec::from(arena, elems))
    }

    /** push an element into a vector. */
    pub fn push(self, value: Dyn<'a>) -> DynResult<'a> {
        match self {
            Dyn::Vec(vec_cell) => vec_cell.push(value),
            _ => panic!(""),
        }
        return Ok(Dyn::Undefined);
    }

    pub fn deref(self) -> Dyn<'a> {
        match self {
            Dyn::Ref(cell) => cell.get(),
            // This should never occur, since we insert refs and derefs in the
            // right places.
            _ => panic!("invoked deref on {:?}", self),
        }
    }

    pub fn void(&self, _arena: &'a Bump) -> DynResult<'a> {
        return Ok(Dyn::Undefined);
    }

    pub fn typeof_(&self, arena: &'a Bump) -> DynResult<'a> {
        match self {
            Dyn::Str(_) => Ok(Dyn::str(arena, "string")),
            Dyn::Bool(_) => Ok(Dyn::str(arena, "boolean")),
            Dyn::Int(_) => Ok(Dyn::str(arena, "number")),
            Dyn::Float(_) => Ok(Dyn::str(arena, "number")),
            Dyn::Undefined => Ok(Dyn::str(arena, "undefined")),
            Dyn::Vec(_) => Ok(Dyn::str(arena, "object")),
            Dyn::Object(_) => Ok(Dyn::str(arena, "object")),
            Dyn::Ref(_) => panic!("typeof_ applied to a ref")
        }
    }

    pub fn to_json(&self) -> Option<serde_json::Value> {
        use serde_json::Value;
        match self {
            Dyn::Str(s) => Some(Value::String(s.to_string())),
            Dyn::Bool(b) => Some(Value::Bool(*b)),
            Dyn::Int(n) => unimplemented!(), // Value::Number(*n.into()),
            Dyn::Float(x) => serde_json::Number::from_f64(*x).map(|num| Value::Number(num)),
            Dyn::Undefined => None,
            Dyn::Vec(vec_cell) => Some(vec_cell.to_json()),
            Dyn::Object(o) => Some(o.to_json()),
            Dyn::Ref(_) => panic!("typeof_ applied to a ref")
        }
    }

    pub fn from_json(arena: &'a Bump, json: serde_json::Value) -> Dyn<'a> {
        use serde_json::Value;
        match json {
            Value::String(s) => Dyn::str(arena, &s),
            Value::Number(n) => {
                // TODO(arjun): do better!
                Dyn::Float(n.as_f64().unwrap())
            }
            Value::Bool(b) => Dyn::Bool(b),
            Value::Null => unimplemented!(),
            Value::Array(vec) => {
                let mut v = Vec::new_in(arena);
                for item in vec.into_iter() {
                    v.push(Self::from_json(arena, item));
                }
                // Why isn't this refcell immediate?
                Dyn::Vec(DynVec { elems: arena.alloc(RefCell::new(v)) })
            }
            Value::Object(key_value_pairs) => {
                let mut obj = Vec::new_in(arena);
                for (k, v) in key_value_pairs.into_iter() {
                    obj.push((
                        String::from_str_in(&k, arena).into_bump_str(),
                        Self::from_json(arena, v),
                    ))
                }
                Dyn::Object(DynObject { fields: arena.alloc(RefCell::new(obj)) })
            }
        }
    }

    pub fn from_json_string(arena: &'a Bump, json_str: &str) -> DynResult<'a> {
        let json_value = serde_json::from_str(json_str)
            .map_err(|e| Error::TypeError(format!("JSON error: {}", e)))?;
        Ok(Dyn::from_json(arena, json_value))
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
            Dyn::Float(n) => Ok(n.to_string()),
            Dyn::Int(n) => Ok(n.to_string()),
            //Dyn::Object(_) => Ok("[object Object]".to_string()),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<Dyn<'a>> for DynObject<'a> {
    type Error = ();

    fn try_from(value: Dyn<'a>) -> Result<Self, ()> {
        match value {
            Dyn::Object(o) => Ok(o),
            _ => Err(()),
        }
    }
}
