use quote::{
    __rt::TokenStream,
    *
};
use syn::{
    Ident
};
use proc_macro2::Span;
use std::collections::HashMap;
use im_rc::{
    HashMap as ImHashMap,
    HashSet as ImHashSet
};

use crate::types::Typ;

fn compile_name(name: &str) -> Ident {
    return Ident::new(&format!("{}", name.to_string()), Span::call_site());
}

fn compile_rust_type_name(name: &usize) -> Ident {
    return compile_name(& ("RustType".to_string() + name.to_string().as_str()));
}

fn compile_variant_name(name: &usize) -> Ident {
    return compile_name(& ("Variant".to_string() + name.to_string().as_str()));
}

fn compile_constructor(input_type: &str, output_type: &Ident, enum_name: &Ident, variant_name: &Ident) -> TokenStream {
    let q_input_type = compile_name(input_type);

    return quote! {
        pub fn #q_input_type(input: #q_input_type) -> #output_type<'a> {
            return Ok(#enum_name::#variant_name(input));
        }
    }
}

// quote! stuff for inside of types.
fn compile_typ(typ: &Typ) -> TokenStream {
    match typ {
        Typ::Bool => quote! { bool },
        Typ::F64 => quote! { f64 },
        Typ::I32 => quote! { i32 },
        Typ::String => quote! { &'a str },
        Typ::Unknown => quote! { Unknown },
        Typ::Undefined => quote! { Undefined },
        Typ::Ref(t) => {
            let q_t = compile_typ(t);
            quote! {
                &'a RefCell<#q_t>
            }
        }
        Typ::RustType(id) => {
            let q_type = compile_rust_type_name(id);
            return quote! {
                #q_type<'a>
            }
        }
        _ => unimplemented!()
    }
}

fn compile_bool_variant(q_name: Ident, q_result_name: Ident, q_variant_name: Ident) -> (TokenStream, TokenStream) {
    let methods = quote! {
        pub fn bool(b: bool) -> #q_result_name<'a> {
            return Ok(#q_name::#q_variant_name(b));
        }

        pub fn strict_eq<T>(&self, other: T) -> BoolResult
            where T : std::convert::Into<bool> {
                match self {
                    #q_name::#q_variant_name(n) => return Ok(Bool(*n == other.into())),
                    _ => return Err(Error::TypeError)
                }
            }
    };

    let impls = quote! {
        impl<'a> std::convert::Into<bool> for #q_name<'a> {
            fn into(self) -> bool {
                match self {
                    #q_name::#q_variant_name(b) => return b,
                    _ => unimplemented!()
                }
            }
        }
    };

    return (methods, impls);
}

fn compile_f64_variant(q_name: Ident, q_result_name: Ident, q_variant_name: Ident) -> (TokenStream, TokenStream) {
    let methods = quote! {
        pub fn f64(n: f64) -> #q_result_name<'a> {
            return Ok(#q_name::#q_variant_name(n));
        }

        pub fn strict_eq<T>(&self, other: T) -> BoolResult
            where T : std::convert::Into<f64> {
                match self {
                    #q_name::#q_variant_name(n) => return Ok(Bool(*n == other.into())),
                    _ => return Err(Error::TypeError)
                }
            }

        pub fn gt<T>(&self, other: T) -> BoolResult
            where T : std::convert::Into<f64> {
                match self {
                    #q_name::#q_variant_name(n) => return Ok(Bool(*n > other.into())),
                    _ => return Err(Error::TypeError)
                }
            }

        pub fn mul<T>(self, other: T) -> #q_result_name<'a>
            where T : std::convert::Into<f64> {
                match self {
                    #q_name::#q_variant_name(n) => return #q_name::f64(n * other.into()),
                    _ => return Err(Error::TypeError)
                }
        }
    };   

    let impls = quote! {
        impl<'a> std::convert::Into<f64> for #q_name<'a> {
            fn into(self) -> f64 {
                match self {
                    #q_name::#q_variant_name(n) => return n,
                    _ => unimplemented!()
                }
            }
        }
    };

    return (methods, impls);
}

fn compile_i32_variant(q_name: Ident, q_result_name: Ident, q_variant_name: Ident) -> (TokenStream, TokenStream) {
    let methods = quote! {
        pub fn i32(i: i32) -> #q_result_name<'a> {
            return Ok(#q_name::#q_variant_name(i));
        }

        pub fn strict_eq<T>(&self, other: T) -> BoolResult
            where T : std::convert::Into<i32> {
                match self {
                    #q_name::#q_variant_name(n) => return Ok(Bool(*n == other.into())),
                    _ => return Err(Error::TypeError)
                }
            }

        pub fn mul<T>(self, other: T) -> #q_result_name<'a>
            where T : std::convert::Into<i32> {
                match self {
                    #q_name::#q_variant_name(n) => return #q_name::i32(n * other.into()),
                    _ => return Err(Error::TypeError)
                }
        }
    };

    let impls = quote! {
        impl<'a> std::convert::Into<i32> for #q_name<'a> {
            fn into(self) -> i32 {
                match self {
                    #q_name::#q_variant_name(i) => return i,
                    _ => unimplemented!()
                }
            }
        }
    };

    return (methods, impls);
}

// enum
fn compile_union(name: &usize, ts: &ImHashSet<Typ>) -> TokenStream {
    let q_name = compile_rust_type_name(name);
    let q_result_name = compile_name(& ("RustType".to_string() + name.to_string().as_str() + "Result"));
    let mut q_methods: Vec<TokenStream> = vec![];
    let mut q_impls: Vec<TokenStream> = vec![];

    let q_ts = ts.iter().enumerate().map(|(i,typ)| {
        let q_variant_name = compile_variant_name(&i);
        let q_typ = compile_typ(typ);
        match typ {
            Typ::Bool => {
                let (methods, impls) = compile_bool_variant(q_name.clone(), q_result_name.clone(), q_variant_name.clone());
                q_methods.push(methods);
                q_impls.push(impls);
                quote! { #q_variant_name(#q_typ) }
            },
            Typ::F64 => {
                let (methods, impls) = compile_f64_variant(q_name.clone(), q_result_name.clone(), q_variant_name.clone());
                q_methods.push(methods);
                q_impls.push(impls);
                quote! { #q_variant_name(#q_typ) }
            },
            Typ::I32 => {
                let (methods, impls) = compile_i32_variant(q_name.clone(), q_result_name.clone(), q_variant_name.clone());
                q_methods.push(methods);
                q_impls.push(impls);
                quote! { #q_variant_name(#q_typ) }
            },
            Typ::String => quote! { #q_variant_name(#q_typ) },
            Typ::Ref(_) => quote! { #q_variant_name(#q_typ) },
            _ => q_typ
        }
    });

    return quote! {
        pub type #q_result_name<'a> = Result<#q_name<'a>,Error>;

        pub enum #q_name<'a> {
            PhantomData(PhantomData<&'a i32>),
            #(#q_ts),*
        }

        impl<'a> #q_name<'a> {
            #(#q_methods)*
        }

        #(#q_impls)*
    };
}

// struct
fn compile_object(name: &usize, tm: &ImHashMap<String, Typ>) -> TokenStream {
    let q_name = compile_rust_type_name(name);

    let q_tm = tm.iter().map(|(field, typ)| {
        let q_field = Ident::new(&format!("{}", field), Span::call_site());
        let q_typ = compile_typ(typ);
        quote! {
            #q_field : #q_typ
        }
    });
    
    return quote! {
        pub struct #q_name<'a> {
            _phantom: PhantomData<&'a i32>,
            #(#q_tm),*
        }
    };
}

// https://github.com/plasma-umass/decontainerization/blob/65d9ee78dc720efac7b6d944d8f70f608024ba2c/native/src/lib.rs#L102
pub fn compile_typs(types: &HashMap<usize, Typ>) -> TokenStream {

    // Only create structs and enums for Unions and Objects.
    let q_typs = types.iter()
        .fold(vec![], |mut acc: Vec<TokenStream>, (name, typ)| {
            match typ {
                Typ::Union(ts) => {
                    acc.push(compile_union(name, ts));
                    return acc;
                },
                Typ::Object(tm) => {
                    acc.push(compile_object(name, tm));
                    return acc;
                },
                _ => {
                    return acc;
                }
            }
        });

    return quote! {
        #[no_mangle]
        #[allow(unused_parens)]
        #[allow(dead_code)]

        use std::cell::RefCell;
        use std::marker::PhantomData;

        // https://github.com/plasma-umass/decontainerization/blob/master/rust/trace-runtime/src/error.rs
        #[derive(Debug)]
        pub enum Error {
            /** Equivalent to a runtime type-error in JavaScript */
            TypeError,
            /** Result of reaching an unknown portion of the trace. */
            Unknown
        }

        impl std::fmt::Display for Error {

            fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
                match self {
                    Error::TypeError => fmt.write_str("TypeError"),
                    Error::Unknown => fmt.write_str("Unknown")
                }
            }

        }

        impl std::error::Error for Error { }

        pub struct I32(i32);

        impl std::convert::Into<i32> for I32 {
            fn into(self) -> i32 {
                return self.0;
            }
        }

        impl I32 {
            pub fn strict_eq<T>(&self, other: T) -> Bool
            where
                T : std::convert::Into<i32>
            {
                return Bool(self.0 == other.into());
            }

            pub fn mul<T>(&self, other: T) -> I32
            where
                T : std::convert::Into<i32>
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
                T : std::convert::Into<f64>
            {
                return Bool(self.0 == other.into());
            }

            pub fn mul<T>(&self, other: T) -> F64
            where
                T : std::convert::Into<f64>
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
                T : std::convert::Into<bool>
            {
                return Bool(self.0 == other.into());
            }
        }

        pub struct Str<'a>(&'a str);

        #(#q_typs)*

    };
}

pub struct JSInt(i32);

impl std::convert::Into<i32> for JSInt {

    fn into(self) -> i32 {
        return self.0;
    }

}

impl JSInt {
    pub fn mul<T>(&self, other: T) -> i32
        where T : std::convert::Into<i32> {
            return self.0 + other.into();
    }
}

#[cfg(test)]
mod tests {

    use std::{
        io::Write,
        collections::HashMap
    };

    use crate::{
        types::{
            Typ,
            constructors::*
        },
        gen::gen_types::compile_typs
    };

    fn test_harness(types: &HashMap<usize, Typ>) {
        let q_types = compile_typs(types);

        let filename = "TYPES.rs";

        let mut rs_file = std::fs::File::create(filename)
            .expect("Could not create .rs file.");
        rs_file.write_all(format!("{}", q_types).as_bytes())
            .expect("Could not write to file.");
        cmd!("rustfmt", filename).run()
            .expect("rustfmt failed");
        cmd!("rustc", "--crate-type", "cdylib", filename).run()
            .expect("Compiling to Rust failed.");
    }

    #[test]
    fn types_1() {
        let mut types = HashMap::new();
        types.insert(0, t_union_2(&[Typ::F64, Typ::Bool]));
        types.insert(1, t_union_2(&[Typ::F64, Typ::String]));

        test_harness(&types);

        assert!(false);
    }

}