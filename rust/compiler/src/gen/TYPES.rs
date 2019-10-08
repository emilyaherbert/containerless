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

Update:

Just kidding scrap everything! This is better:
https://stackoverflow.com/questions/26412396/several-implementations-of-the-add-trait-for-the-same-type

Apparently you are allowed to implement traits for multiple types, but you are
allowed one implementation for trait objects... I think this might actually be a
bug.

*/

trait JSDynamic {
    fn to_i32(&self) -> I32Result {
        return Err(Error::TypeError);
    }

    fn to_f64(&self) -> F64Result {
        return Err(Error::TypeError);
    }

    fn to_bool(&self) -> BoolResult {
        return Err(Error::TypeError);
    }
}

trait Add<other=Self> {
    type Output;

    fn add(&self, other: &other) -> Self::Output;
}

/*
    #################
    #      I32      #
    #################
*/
#[derive(Debug)]
pub struct I32(i32);
pub type I32Result = Result<I32, Error>;
impl I32 {
    pub fn i32(n: i32) -> I32Result {
        return Ok(I32(n));
    }

    pub fn to_i32(&self) -> i32 {
        return self.0;
    }
}
impl Add for I32 {
    type Output = I32Result;

    fn add(&self, other: &I32) -> Self::Output {
        return I32::i32(self.0 + other.0);
    }
}
impl<T> Add<T> for I32 where T: JSDynamic {
    type Output = I32Result;

    fn add(&self, other: &T) -> Self::Output {
        return I32::i32(self.0 + other.to_i32()?.0);
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
impl F64 {
    pub fn f64(n: f64) -> F64Result {
        return Ok(F64(n));
    }

    pub fn to_f64(&self) -> f64 {
        return self.0;
    }
}
impl Add for F64 {
    type Output = F64Result;

    fn add(&self, other: &F64) -> Self::Output {
        return F64::f64(self.0 + other.0);
    }
}
impl<T> Add<T> for F64 where T: JSDynamic {
    type Output = F64Result;

    fn add(&self, other: &T) -> Self::Output {
        return F64::f64(self.0 + other.to_f64()?.0);
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
impl Bool {
    pub fn bool(b: bool) -> BoolResult {
        return Ok(Bool(b));
    }

    pub fn to_bool(&self) -> bool {
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
impl<'a> JSDynamic for RustType0<'a> {
    fn to_f64(&self) -> F64Result {
        match self {
            RustType0::Variant1(n) => F64::f64(*n),
            _ => Err(Error::TypeError)
        }
    }

    fn to_bool(&self) -> BoolResult {
        match self {
            RustType0::Variant0(n) => Bool::bool(*n),
            _ => Err(Error::TypeError)
        }
    }
}
impl<'a> Add<F64> for RustType0<'a> {
    type Output = RustType0Result<'a>;

    fn add(&self, other: &F64) -> Self::Output {
        match self {
            RustType0::Variant1(n) => RustType0::f64(n + other.to_f64()),
            _ => Err(Error::TypeError)
        }
    }
}
impl<'a, T> Add<T> for RustType0<'a> where T: JSDynamic {
    type Output = RustType0Result<'a>;

    fn add(&self, other: &T) -> Self::Output {
        match self {
            RustType0::Variant1(n) => RustType0::f64(n + other.to_f64()?.0),
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
impl<'a> JSDynamic for RustType1<'a> {
    fn to_i32(&self) -> I32Result {
        match self {
            RustType1::Variant0(n) => I32::i32(*n),
            _ => Err(Error::TypeError)
        }
    }

    fn to_f64(&self) -> F64Result {
        match self {
            RustType1::Variant1(n) => F64::f64(*n),
            _ => Err(Error::TypeError)
        }
    }
}
impl<'a> Add<I32> for RustType1<'a> {
    type Output = RustType1Result<'a>;

    fn add(&self, other: &I32) -> Self::Output {
        match self {
            RustType1::Variant0(n) => RustType1::i32(n + other.to_i32()),
            _ => Err(Error::TypeError)
        }
    }
}
impl<'a> Add<F64> for RustType1<'a> {
    type Output = RustType1Result<'a>;

    fn add(&self, other: &F64) -> Self::Output {
        match self {
            RustType1::Variant1(n) => RustType1::f64(n + other.to_f64()),
            _ => Err(Error::TypeError)
        }
    }
}
impl<'a, T> Add<T> for RustType1<'a> where T: JSDynamic {
    type Output = RustType1Result<'a>;

    fn add(&self, other: &T) -> Self::Output {
        match self {
            RustType1::Variant0(n) => RustType1::i32(n + other.to_i32()?.0),
            RustType1::Variant1(n) => RustType1::f64(n + other.to_f64()?.0),
            _ => Err(Error::TypeError)
        }
    }
}

#[cfg(test)]
mod tests {

    use crate::gen::TYPES::*;

    //#[test]
    fn trying_types() {
        let a = F64(1.0);
        let b = F64(10.0);
        let c = Bool(true);
        let mut x = RustType0::f64(100.0).unwrap();
        let y = RustType1::f64(1000.0).unwrap();
        let z = RustType1::i32(2).unwrap();

        let s1 = a.add(&b).unwrap();
        println!("{:?}", s1);
        let s2 = a.add(&y).unwrap();
        println!("{:?}", s2);
        let s3 = y.add(&a).unwrap();
        println!("{:?}", s3);
        let s4 = y.add(&x).unwrap();
        println!("{:?}", s4);
        let s5 = y.add(&y).unwrap();
        println!("{:?}", s5);
        let s6 = y.add(&z).unwrap();
        println!("{:?}", s6);
        
        
        //let fail = a.add(&c).unwrap();
    }

}