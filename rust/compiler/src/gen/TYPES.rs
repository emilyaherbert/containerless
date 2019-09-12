#[no_mangle]
#[allow(unused_parens)]
#[allow(dead_code)]
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
trait StrictEq<T> {
    fn strict_eq<U>(self, other: U) -> Bool
    where
        U: std::convert::Into<T>;
}

trait MayConvertToF64 {
    fn to_f64(&self) -> f64;
}

trait MayConvertToBool {
    fn to_f64(&self) -> Bool;
}

trait StrictEq2<T> {
    fn strict_eq2(self, other: T) -> Bool;
}

pub struct I32(i32);
impl std::convert::Into<i32> for I32 {
    fn into(self) -> i32 {
        return self.0;
    }
}
impl I32 {
    pub fn strict_eq<T>(&self, other: T) -> Bool
    where
        T: std::convert::Into<i32>,
    {
        return Bool(self.0 == other.into());
    }
    pub fn mul<T>(&self, other: T) -> I32
    where
        T: std::convert::Into<i32>,
    {
        return I32(self.0 * other.into());
    }
}

pub struct F64(f64);
impl std::convert::Into<f64> for F64 {
    fn into(self) -> f64 {
        return self.0;
    }
}
impl F64 {
    pub fn strict_eq<T>(&self, other: T) -> Bool
    where
        T: std::convert::Into<f64>,
    {
        return Bool(self.0 == other.into());
    }
    pub fn mul<T>(&self, other: T) -> F64
    where
        T: std::convert::Into<f64>,
    {
        return F64(self.0 * other.into());
    }
}

pub type BoolResult<'a> = Result<Bool, Error>;

pub struct Bool(bool);
impl std::convert::Into<bool> for Bool {
    fn into(self) -> bool {
        return self.0;
    }
}
impl Bool {
    pub fn strict_eq<T>(&self, other: T) -> Bool
    where
        T: std::convert::Into<bool>,
    {
        return Bool(self.0 == other.into());
    }
}

pub struct Str<'a>(&'a str);

pub type RustType0Result<'a> = Result<RustType0<'a>, Error>;
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
impl<'a> std::convert::Into<bool> for RustType0<'a> {
    fn into(self) -> bool {
        match self {
            RustType0::Variant0(b) => return b,
            _ => unimplemented!(),
        }
    }
}
impl<'a> std::convert::Into<f64> for RustType0<'a> {
    fn into(self) -> f64 {
        match self {
            RustType0::Variant1(b) => return b,
            _ => unimplemented!(),
        }
    }
}
impl<'a> StrictEq<bool> for RustType0<'a> {
    fn strict_eq<T>(self, other: T) -> Bool
    where
        T: std::convert::Into<bool>,
    {
        match self {
            RustType0::Variant0(b) => return Bool(b == other.into()),
            _ => unimplemented!(),
        }
    }
}
impl<'a> StrictEq<f64> for RustType0<'a> {
    fn strict_eq<T>(self, other: T) -> Bool
    where
        T: std::convert::Into<f64>,
    {
        match self {
            RustType0::Variant1(b) => return Bool(b == other.into()),
            _ => unimplemented!(),
        }
    }
}
/*
impl<'a> MayConvertToF64 for RustType0<'a> {
    fn to_f64(&self) -> f64 {
        match self {
            RustType0::Variant1(i) => return *i,
            _ => unimplemented!()
        }
    }
}
impl<'a, T: MayConvertToBool> StrictEq2<T> for RustType0<'a> {
    fn strict_eq2(self, other: T) -> Bool {
        match self {
            RustType0::Variant0(b) => return Bool(b == other.to_f64()),
            _ => unimplemented!()
        }
    }
}
impl<'a, T: MayConvertToF64> StrictEq2<T> for RustType0<'a> {
    fn strict_eq2(self, other: T) -> Bool {
        match self {
            RustType0::Variant1(i) => return Bool(i == other.to_f64()),
            _ => unimplemented!()
        }
    }
}
*/

pub type RustType1Result<'a> = Result<RustType1<'a>, Error>;
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
impl<'a> std::convert::Into<i32> for RustType1<'a> {
    fn into(self) -> i32 {
        match self {
            RustType1::Variant0(b) => return b,
            _ => unimplemented!(),
        }
    }
}
impl<'a> std::convert::Into<f64> for RustType1<'a> {
    fn into(self) -> f64 {
        match self {
            RustType1::Variant1(b) => return b,
            _ => unimplemented!(),
        }
    }
}
impl<'a> StrictEq<f64> for RustType1<'a> {
    fn strict_eq<T>(self, other: T) -> Bool
    where
        T: std::convert::Into<f64>,
    {
        match self {
            RustType1::Variant1(b) => return Bool(b == other.into()),
            _ => unimplemented!(),
        }
    }
}
impl<'a> MayConvertToF64 for RustType1<'a> {
    fn to_f64(&self) -> f64 {
        match self {
            RustType1::Variant1(i) => return *i,
            _ => unimplemented!(),
        }
    }
}
impl<'a, T: MayConvertToF64> StrictEq2<T> for RustType1<'a> {
    fn strict_eq2(self, other: T) -> Bool {
        match self {
            RustType1::Variant1(i) => return Bool(i == other.to_f64()),
            _ => unimplemented!(),
        }
    }
}

fn test_fun() {
    let a = F64(3.0);
    let b = F64(3.0);
    let c = a.mul(b);

    let x = RustType0::f64(1.0).unwrap();
    let y = RustType1::f64(2.0).unwrap();

    // Can't disimbiguate between StrictEq implementations.
    //let the_same = x.strict_eq(y);

    // Can't implement StrictEq2 more than once, because there can only be one implementation that accepts any trait ?
    //let the_same_2 = x.strict_eq2(y);
}
