#[no_mangle]
#[allow(unused_parens)]
#[allow(dead_code)]
#[allow(unused_variables)]

use std::cell::RefCell;
use std::marker::PhantomData;
#[derive(Debug)]
pub enum Error {
    #[doc = r" Equivalent to a runtime type-error in JavaScript "]
    TypeError,
    #[doc = r" Result of reaching an unknown portion of the trace. "]
    Unknown,
}
impl std::fmt::Display for Error {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Error::TypeError => fmt.write_str("TypeError"),
            Error::Unknown => fmt.write_str("Unknown"),
        }
    }
}
impl std::error::Error for Error {}

// https://www.reddit.com/r/rust/comments/7zrycu/so_function_overloading_is_part_of_stable_rust/
// http://smallcultfollowing.com/babysteps/blog/2014/09/30/multi-and-conditional-dispatch-in-traits/
// https://tratt.net/laurie/blog/entries/a_quick_look_at_trait_objects_in_rust.html

/*

Key insight:

use dyn !
and Self !
no generics !

https://tratt.net/laurie/blog/entries/a_quick_look_at_trait_objects_in_rust.html
https://doc.rust-lang.org/book/ch17-02-trait-objects.html#object-safety-is-required-for-trait-objects

*/

trait Dynamic {
    fn get_i32(&self) -> I32Result {
        return Err(Error::TypeError);
    }

    fn get_f64(&self) -> F64Result {
        return Err(Error::TypeError);
    }

    fn get_bool(&self) -> BoolResult {
        return Err(Error::TypeError);
    }

    fn strict_eq(&self, other: &dyn Dynamic) -> BoolResult {
        return Err(Error::TypeError);
    }

    fn add(&self, other: &dyn Dynamic) -> Result<Self, Error> where Self: std::marker::Sized {
        return Err(Error::TypeError);
    }
}

/*
    #################
    #      I32      #
    #################
*/
#[derive(Debug)]
pub struct I32(i32);
pub type I32Result = Result<I32, Error>;
trait MayConvertToI32 {
    fn to_i32(&self) -> i32;
}
impl I32 {
    pub fn i32(n: i32) -> I32Result {
        return Ok(I32(n));
    }
    pub fn strict_eq(&self, other: &dyn MayConvertToI32) -> BoolResult {
        return Bool::bool(self.0 == other.to_i32());
    }
    pub fn add(&self, other: &dyn MayConvertToI32) -> I32Result {
        return I32::i32(self.0 + other.to_i32());
    }
    pub fn sub(&self, other: &dyn MayConvertToI32) -> I32Result {
        return I32::i32(self.0 - other.to_i32());
    }
    pub fn mul(&self, other: &dyn MayConvertToI32) -> I32Result {
        return I32::i32(self.0 * other.to_i32());
    }
    pub fn div(&self, other: &dyn MayConvertToI32) -> I32Result {
        return I32::i32(self.0 / other.to_i32());
    }
}
impl std::convert::Into<i32> for I32 {
    fn into(self) -> i32 {
        return self.0;
    }
}


/*
    #################
    #      F64      #
    #################
*/
#[derive(Debug)]
pub struct F64(f64);
pub type F64Result = Result<F64, Error>;
trait MayConvertToF64 {
    fn to_f64(&self) -> f64;
}
impl F64 {
    pub fn f64(n: f64) -> F64Result {
        return Ok(F64(n));
    }
    pub fn strict_eq(&self, other: &dyn MayConvertToF64) -> BoolResult {
        return Bool::bool(self.0 == other.to_f64());
    }
    pub fn add(&self, other: &dyn MayConvertToF64) -> F64Result {
        return F64::f64(self.0 + other.to_f64());
    }
    pub fn sub(&self, other: &dyn MayConvertToF64) -> F64Result {
        return F64::f64(self.0 - other.to_f64());
    }
    pub fn mul(&self, other: &dyn MayConvertToF64) -> F64Result {
        return F64::f64(self.0 * other.to_f64());
    }
    pub fn div(&self, other: &dyn MayConvertToF64) -> F64Result {
        return F64::f64(self.0 / other.to_f64());
    }
}
impl MayConvertToF64 for F64 {
    fn to_f64(&self) -> f64 {
        return self.0;
    }
}
impl Dynamic for F64 {
    fn get_f64(&self) -> F64Result {
        return F64::f64(self.0);
    }

    fn strict_eq(&self, other: &dyn Dynamic) -> BoolResult {
        return Bool::bool(self.0 == other.get_f64()?.0);
    }

    fn add(&self, other: &dyn Dynamic) -> F64Result
        where Self: std::marker::Sized {
        return F64::f64(self.0 + other.get_f64()?.0);
    }
}

/*
    #################
    #      Bool     #
    #################
*/
#[derive(Debug)]
pub struct Bool(bool);
pub type BoolResult = Result<Bool, Error>;
trait MayConvertToBool {
    fn to_bool(&self) -> bool;
}
impl Bool {
    pub fn bool(b: bool) -> BoolResult {
        return Ok(Bool(b));
    }
    pub fn strict_eq(&self, other: &dyn MayConvertToBool) -> BoolResult {
        return Bool::bool(self.0 == other.to_bool());
    }
    pub fn and(&self, other: &dyn MayConvertToBool) -> BoolResult {
        return Bool::bool(self.0 && other.to_bool());
    }
    pub fn or(&self, other: &dyn MayConvertToBool) -> BoolResult {
        return Bool::bool(self.0 || other.to_bool());
    }
}
impl std::convert::Into<bool> for Bool {
    fn into(self) -> bool {
        return self.0;
    }
}

/*
    #################
    #   RustType0   #
    #################
*/
pub type RustType0Result<'a> = Result<RustType0<'a>, Error>;
#[derive(Debug)]
pub enum RustType0<'a> {
    PhantomData(PhantomData<&'a i32>),
    Variant0(bool),
    Variant1(f64),
}
impl<'a> RustType0<'a> {
    pub fn bool(i: bool) -> RustType0Result<'a> {
        return Ok(RustType0::Variant0(i));
    }
    pub fn f64(i: f64) -> RustType0Result<'a> {
        return Ok(RustType0::Variant1(i));
    }
}
impl<'a> Dynamic for RustType0<'a> {
    fn get_f64(&self) -> F64Result {
        match self {
            RustType0::Variant1(n) => F64::f64(*n),
            _ => Err(Error::TypeError)
        }
    }

    fn get_bool(&self) -> BoolResult {
        match self {
            RustType0::Variant0(n) => Bool::bool(*n),
            _ => Err(Error::TypeError)
        }
    }

    fn strict_eq(&self, other: &dyn Dynamic) -> BoolResult {
        match self {
            RustType0::Variant0(b) => Bool::bool(*b == other.get_bool()?.0),
            RustType0::Variant1(n) => Bool::bool(*n == other.get_f64()?.0),
            _ => Err(Error::TypeError)
        }
    }

    fn add(&self, other: &dyn Dynamic) -> RustType0Result<'a> where Self: std::marker::Sized {
        match self {
            RustType0::Variant1(n) => RustType0::f64(n + other.get_f64()?.0),
            _ => Err(Error::TypeError)
        }
    }
}

/*
    #################
    #   RustType1   #
    #################
*/
pub type RustType1Result<'a> = Result<RustType1<'a>, Error>;
#[derive(Debug)]
pub enum RustType1<'a> {
    PhantomData(PhantomData<&'a i32>),
    Variant0(i32),
    Variant1(f64),
}
impl<'a> RustType1<'a> {
    pub fn i32(i: i32) -> RustType1Result<'a> {
        return Ok(RustType1::Variant0(i));
    }
    pub fn f64(i: f64) -> RustType1Result<'a> {
        return Ok(RustType1::Variant1(i));
    }
}
impl<'a> Dynamic for RustType1<'a> {
    fn get_i32(&self) -> I32Result {
        match self {
            RustType1::Variant0(n) => I32::i32(*n),
            _ => Err(Error::TypeError)
        }
    }

    fn get_f64(&self) -> F64Result {
        match self {
            RustType1::Variant1(n) => F64::f64(*n),
            _ => Err(Error::TypeError)
        }
    }

    fn strict_eq(&self, other: &dyn Dynamic) -> BoolResult {
        match self {
            RustType1::Variant0(n) => Bool::bool(*n == other.get_i32()?.0),
            RustType1::Variant1(n) => Bool::bool(*n == other.get_f64()?.0),
            _ => Err(Error::TypeError)
        }
    }

    fn add(&self, other: &dyn Dynamic) -> RustType1Result<'a> where Self: std::marker::Sized {
        match self {
            RustType1::Variant0(n) => RustType1::i32(n + other.get_i32()?.0),
            RustType1::Variant1(n) => RustType1::f64(n + other.get_f64()?.0),
            _ => Err(Error::TypeError)
        }
    }
}

#[cfg(test)]
mod tests {

    use crate::gen::TYPES::*;

    #[test]
    fn trying_types() {
        let a = F64(1.0);
        let b = F64(10.0);
        let c = a.mul(&b);

        let foo = Bool(true);
        //let fail = a.add(foo);

        let mut x = RustType0::f64(100.0).unwrap();
        let y = RustType1::f64(1000.0).unwrap();

        // primitive + primitive
        // "static"
        let r1 = a.strict_eq(&b).unwrap();
        let s1 = a.add(&b).unwrap();
        println!("{:?}", s1);

        // rust type + rust type
        // "dynamic"
        let r2 = x.strict_eq(&y).unwrap();
        let s2 = y.add(&x).unwrap();
        println!("{:?}", s2);

        // rust type + primitive
        // "dynamic"
        let r3 = x.strict_eq(&b).unwrap();
        let s3 = y.add(&a).unwrap();
        println!("{:?}", s3);

        // primitive + rust type
        // "dynamic"
        //let r4 = b.strict_eq(y);
        //let s3 = b.add(y).unwrap();
    }

}