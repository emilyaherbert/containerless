//! Dynamic type wrapper.

use super::error::{not_a_function, type_error, Error};
use bumpalo::{
    collections::{String, Vec},
    Bump,
};
use std::cell::{Cell, RefCell};
use std::convert::{From, TryFrom};

#[allow(unused)]
pub fn unknown<'a>() -> DynResult<'a> {
    Err(Error::Unknown)
}

#[derive(Debug, Copy, Clone)]
pub struct DynObject<'a> {
    fields: &'a RefCell<Vec<'a, (&'a str, Dyn<'a>)>>,
}

impl<'a> DynObject<'a> {
    pub fn new(arena: &'a Bump) -> DynObject<'a> {
        DynObject {
            fields: arena.alloc(RefCell::new(Vec::new_in(arena))),
        }
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
            if k.clone() == key.to_string() {
                *v = value;
                return;
            }
        }
        vec.push((key, value));
    }

    pub fn get(&self, key: &str) -> Dyn<'a> {
        let vec = self.fields.borrow();
        let mut proto = None;
        for (k, v) in vec.iter() {
            if *k == key {
                return *v;
            }
            if *k == "__proto__" {
                proto = Some(*v);
            }
        }
        match proto {
            Some(Dyn::Object(p)) => return p.get(key),
            _ => return Dyn::Undefined,
        }
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
    elems: &'a RefCell<Vec<'a, Dyn<'a>>>,
}

impl<'a> DynVec<'a> {
    pub fn new(arena: &'a Bump) -> DynVec<'a> {
        DynVec {
            elems: arena.alloc(RefCell::new(Vec::new_in(arena))),
        }
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
            _ => Dyn::Undefined,
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

    pub fn pop(self) -> Dyn<'a> {
        self.elems.borrow_mut().pop().unwrap_or(Dyn::Undefined)
    }

    pub fn shift(self) -> Dyn<'a> {
        let mut ret = Dyn::Undefined;
        match self.elems.borrow().first() {
            Some(e) => ret = e.to_owned(),
            None => return ret,
        }
        self.elems.borrow_mut().remove(0);
        return ret.to_owned();
    }

    pub fn to_json(&self) -> serde_json::Value {
        use serde_json::Value;
        Value::Array(
            self.elems
                .borrow()
                .iter()
                .filter_map(|x| x.to_json())
                .collect(),
        )
    }

    pub fn to_string(&self) -> std::string::String {
        // https://stackoverflow.com/a/42661287
        return self
            .elems
            .borrow()
            .iter()
            .fold(std::string::String::new(), |acc, num| {
                acc + &num.to_string() + ","
            });
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
        return type_error(format!("setref on {:?}", self));
    }

    pub fn set(&mut self, index: Dyn<'a>, new_value: Dyn<'a>) -> DynResult<'a> {
        match (self, index) {
            (Dyn::Vec(v), Dyn::Float(n)) => {
                std::mem::replace(&mut v.elems.borrow_mut()[n as usize], new_value);
                return Ok(Dyn::Undefined);
            }
            _ => type_error("Should only use index on a vec!"),
        }
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
            Dyn::Str(s) => match key {
                "length" => Ok(Dyn::int(s.len() as i32)),
                k => not_a_function(k),
            },
            _ => type_error(format!("{:?} is not an object", self)),
        }
    }

    pub fn add(&self, arena: &'a Bump, other: Dyn<'a>) -> DynResult<'a> {
        match (*self, other) {
            (Dyn::Int(m), Dyn::Int(n)) => Ok(Dyn::Int(m + n)),
            (Dyn::Float(x), Dyn::Int(n)) => Ok(Dyn::Float(x + f64::from(n))),
            (Dyn::Int(n), Dyn::Float(x)) => Ok(Dyn::Float(f64::from(n) + x)),
            (Dyn::Float(x), Dyn::Float(y)) => Ok(Dyn::Float(x + y)),
            (Dyn::Float(x), Dyn::Str(s)) => Ok(Dyn::str(arena, &(x.to_string() + s))),
            (Dyn::Vec(v), Dyn::Str(s)) => Ok(Dyn::str(arena, &(v.to_string() + s))),
            (Dyn::Str(s), Dyn::Vec(v)) => Ok(Dyn::str(arena, &(s.to_string() + &v.to_string()))),
            (Dyn::Str(a), Dyn::Str(b)) => Ok(Dyn::str(arena, &(a.to_string() + b))),
            (Dyn::Undefined, Dyn::Str(s)) => Ok(Dyn::str(arena, &(self.to_string() + s))),
            _ => type_error(&format!("({:?}).add({:?})", &self, &other)),
        }
    }

    pub fn sub(&self, other: Dyn<'a>) -> DynResult<'a> {
        match (*self, other) {
            (Dyn::Int(m), Dyn::Int(n)) => Ok(Dyn::Int(m - n)),
            (Dyn::Float(x), Dyn::Int(n)) => Ok(Dyn::Float(x - f64::from(n))),
            (Dyn::Int(n), Dyn::Float(x)) => Ok(Dyn::Float(f64::from(n) - x)),
            (Dyn::Float(x), Dyn::Float(y)) => Ok(Dyn::Float(x - y)),
            _ => type_error(&format!("({:?}).sub({:?})", &self, &other)),
        }
    }

    pub fn mul(&self, other: Dyn<'a>) -> DynResult<'a> {
        match (*self, other) {
            (Dyn::Int(m), Dyn::Int(n)) => Ok(Dyn::Int(m * n)),
            (Dyn::Float(x), Dyn::Int(n)) => Ok(Dyn::Float(x * f64::from(n))),
            (Dyn::Int(n), Dyn::Float(x)) => Ok(Dyn::Float(f64::from(n) * x)),
            (Dyn::Float(x), Dyn::Float(y)) => Ok(Dyn::Float(x * y)),
            _ => type_error(&format!("({:?}).mul({:?})", &self, &other)),
        }
    }

    pub fn div(&self, other: Dyn<'a>) -> DynResult<'a> {
        match (*self, other) {
            (Dyn::Int(m), Dyn::Int(n)) => Ok(Dyn::Int(m / n)),
            (Dyn::Float(x), Dyn::Int(n)) => Ok(Dyn::Float(x / f64::from(n))),
            (Dyn::Int(n), Dyn::Float(x)) => Ok(Dyn::Float(f64::from(n) / x)),
            (Dyn::Float(x), Dyn::Float(y)) => Ok(Dyn::Float(x / y)),
            _ => type_error(&format!("({:?}).div({:?})", &self, &other)),
        }
    }

    pub fn strict_eq(&self, other: Dyn<'a>) -> DynResult<'a> {
        match (*self, other) {
            (Dyn::Undefined, Dyn::Undefined) => Ok(Dyn::Bool(true)),
            (Dyn::Undefined, _) => Ok(Dyn::Bool(false)),
            (_, Dyn::Undefined) => Ok(Dyn::Bool(false)),
            (Dyn::Int(m), Dyn::Int(n)) => Ok(Dyn::Bool(m == n)),
            (Dyn::Str(s1), Dyn::Str(s2)) => Ok(Dyn::Bool(s1 == s2)),
            (Dyn::Float(x), Dyn::Float(y)) => Ok(Dyn::Bool(x == y)),
            _ => type_error(&format!("({:?}).strict_eq({:?})", &self, &other)),
        }
    }

    pub fn strict_neq(&self, other: Dyn<'a>) -> DynResult<'a> {
        match (*self, other) {
            (Dyn::Undefined, Dyn::Undefined) => Ok(Dyn::Bool(false)),
            (Dyn::Undefined, _) => Ok(Dyn::Bool(true)),
            (_, Dyn::Undefined) => Ok(Dyn::Bool(true)),
            (Dyn::Int(m), Dyn::Int(n)) => Ok(Dyn::Bool(m != n)),
            (Dyn::Str(s1), Dyn::Str(s2)) => Ok(Dyn::Bool(s1 != s2)),
            (Dyn::Float(x), Dyn::Float(y)) => Ok(Dyn::Bool(x != y)),
            _ => type_error(&format!("({:?}).strict_neq({:?})", &self, &other)),
        }
    }

    pub fn gt(&self, other: Dyn<'a>) -> DynResult<'a> {
        match (*self, other) {
            (Dyn::Int(m), Dyn::Int(n)) => Ok(Dyn::Bool(m > n)),
            (Dyn::Float(x), Dyn::Float(y)) => Ok(Dyn::Bool(x > y)),
            (Dyn::Int(m), Dyn::Float(n)) => Ok(Dyn::Bool((m as f64) > n)),
            (Dyn::Float(m), Dyn::Int(n)) => Ok(Dyn::Bool(m > (n as f64))),
            _ => type_error(&format!("({:?}).gt({:?})", &self, &other)),
        }
    }

    pub fn lt(&self, other: Dyn<'a>) -> DynResult<'a> {
        match (*self, other) {
            (Dyn::Int(m), Dyn::Int(n)) => Ok(Dyn::Bool(m < n)),
            (Dyn::Float(x), Dyn::Float(y)) => Ok(Dyn::Bool(x < y)),
            (Dyn::Int(m), Dyn::Float(n)) => Ok(Dyn::Bool((m as f64) < n)),
            (Dyn::Float(m), Dyn::Int(n)) => Ok(Dyn::Bool(m < (n as f64))),
            _ => type_error(format!("({:?}).lt({:?})", self, other)),
        }
    }

    pub fn gte(&self, other: Dyn<'a>) -> DynResult<'a> {
        match (*self, other) {
            (Dyn::Int(m), Dyn::Int(n)) => Ok(Dyn::Bool(m >= n)),
            (Dyn::Float(x), Dyn::Float(y)) => Ok(Dyn::Bool(x >= y)),
            (Dyn::Int(m), Dyn::Float(n)) => Ok(Dyn::Bool((m as f64) >= n)),
            (Dyn::Float(m), Dyn::Int(n)) => Ok(Dyn::Bool(m >= (n as f64))),
            _ => type_error(&format!("({:?}).gte({:?})", &self, &other)),
        }
    }

    pub fn lte(&self, other: Dyn<'a>) -> DynResult<'a> {
        match (*self, other) {
            (Dyn::Int(m), Dyn::Int(n)) => Ok(Dyn::Bool(m <= n)),
            (Dyn::Float(x), Dyn::Float(y)) => Ok(Dyn::Bool(x <= y)),
            (Dyn::Int(m), Dyn::Float(n)) => Ok(Dyn::Bool((m as f64) <= n)),
            (Dyn::Float(m), Dyn::Int(n)) => Ok(Dyn::Bool(m <= (n as f64))),
            _ => type_error(&format!("({:?}).lte({:?})", &self, &other)),
        }
    }

    pub fn and(&self, other: Dyn<'a>) -> DynResult<'a> {
        match (*self, other) {
            (Dyn::Bool(x), Dyn::Bool(y)) => Ok(Dyn::Bool(x && y)),
            _ => type_error(&format!("({:?}).and({:?})", &self, &other)),
        }
    }

    pub fn or(&self, other: Dyn<'a>) -> DynResult<'a> {
        match (*self, other) {
            (Dyn::Bool(x), Dyn::Bool(y)) => Ok(Dyn::Bool(x || y)),
            _ => type_error(&format!("({:?}).or({:?})", &self, &other)),
        }
    }

    /** Array indexing. */
    pub fn index(&self, arena: &'a Bump, index: Dyn<'a>) -> DynResult<'a> {
        match (self, index) {
            (Dyn::Vec(vec_cell), Dyn::Int(index)) => Ok(vec_cell.index(index)),
            (Dyn::Vec(vec_cell), Dyn::Float(index)) => Ok(vec_cell.index(index as i32)),
            (Dyn::Str(s), Dyn::Float(index)) => Ok(Dyn::str(
                arena,
                &s.chars().nth(index as usize).unwrap().to_string(),
            )),
            _ => type_error(&format!("({:?}).index({:?})", &self, &index)),
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
            Dyn::Vec(vec_cell) => {
                vec_cell.push(value);
                Ok(Dyn::Undefined)
            }
            _ => not_a_function("push"),
        }
    }

    pub fn pop(self) -> DynResult<'a> {
        match self {
            Dyn::Vec(vec_cell) => Ok(vec_cell.pop()),
            _ => not_a_function("pop"),
        }
    }

    #[allow(non_snake_case)]
    pub fn startsWith(&self, value: Dyn<'a>) -> DynResult<'a> {
        match (self, value) {
            (Dyn::Str(s), Dyn::Str(prefix)) => Ok(Dyn::Bool(s.starts_with(prefix.as_str()))),
            _ => not_a_function("startsWith"),
        }
    }

    pub fn shift(self) -> DynResult<'a> {
        match self {
            Dyn::Vec(v) => Ok(v.shift()),
            _ => not_a_function("shift"),
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
            Dyn::Ref(_) => panic!("typeof_ applied to a ref"),
        }
    }

    pub fn neg(&self) -> DynResult<'a> {
        match self {
            Dyn::Int(n) => Ok(Dyn::int(-n)),
            Dyn::Float(n) => Ok(Dyn::float(-n)),
            _ => type_error(&format!("({:?}).neg()", &self)),
        }
    }

    pub fn to_json(&self) -> Option<serde_json::Value> {
        use serde_json::Value;
        match self {
            Dyn::Str(s) => Some(Value::String(s.to_string())),
            Dyn::Bool(b) => Some(Value::Bool(*b)),
            Dyn::Int(n) => serde_json::Number::from_f64(*n as f64).map(|num| Value::Number(num)),
            Dyn::Float(x) => serde_json::Number::from_f64(*x).map(|num| Value::Number(num)),
            Dyn::Undefined => None,
            Dyn::Vec(vec_cell) => Some(vec_cell.to_json()),
            Dyn::Object(o) => Some(o.to_json()),
            Dyn::Ref(_) => panic!("typeof_ applied to a ref"),
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
                Dyn::Vec(DynVec {
                    elems: arena.alloc(RefCell::new(v)),
                })
            }
            Value::Object(key_value_pairs) => {
                let mut obj = Vec::new_in(arena);
                for (k, v) in key_value_pairs.into_iter() {
                    obj.push((
                        String::from_str_in(&k, arena).into_bump_str(),
                        Self::from_json(arena, v),
                    ))
                }
                Dyn::Object(DynObject {
                    fields: arena.alloc(RefCell::new(obj)),
                })
            }
        }
    }

    pub fn from_json_string(arena: &'a Bump, json_str: &str) -> DynResult<'a> {
        let json_value = serde_json::from_str(json_str)
            .map_err(|e| Error::TypeError(format!("JSON error: {}", e)))?;
        Ok(Dyn::from_json(arena, json_value))
    }

    pub fn to_string(&self) -> std::string::String {
        match self {
            Dyn::Int(n) => n.to_string(),
            Dyn::Float(n) => n.to_string(),
            Dyn::Bool(b) => b.to_string(),
            Dyn::Str(s) => s.to_string(),
            Dyn::Undefined => "undefined".to_string(),
            Dyn::Ref(cell) => format!("&{}", cell.get().to_string()),
            Dyn::Vec(v) => v.to_string(),
            Dyn::Object(_) => "[object Object]".to_string(),
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
            _ => panic!("Could not convert to bool."),
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
        Ok(value.to_string())
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
