#![allow(unused)]
use im_rc::{HashMap as ImHashMap, HashSet as ImHashSet};
use proc_macro2::Span;
use quote::{__rt::TokenStream, *};
use std::collections::HashMap;
use syn::Ident;

use crate::types::Typ;

#[derive(Clone, Debug)]
struct Variant {
    name: Ident,
    typ: TokenStream,
}

impl Variant {
    pub fn new(name: Ident, typ: TokenStream) -> Variant {
        return Variant {
            name: name,
            typ: typ,
        };
    }
}

#[derive(Debug)]
struct RustTypeEnum {
    name: Ident,
    result_name: Ident,
    variants: Vec<Variant>,
    muls: Vec<Variant>,
    strict_eqs: Vec<Variant>,
}

impl RustTypeEnum {
    pub fn new(name: Ident, result_name: Ident) -> RustTypeEnum {
        return RustTypeEnum {
            name: name,
            result_name: result_name,
            variants: vec![],
            muls: vec![],
            strict_eqs: vec![],
        };
    }
}

fn quote_name(name: &str) -> Ident {
    return Ident::new(&format!("{}", name.to_string()), Span::call_site());
}

fn quote_rust_type_name(name: &usize) -> Ident {
    return quote_name(&("RustType".to_string() + name.to_string().as_str()));
}

fn quote_variant_name(name: &usize) -> Ident {
    return quote_name(&("Variant".to_string() + name.to_string().as_str()));
}

fn quote_constructor(
    input_type: &str,
    output_type: &Ident,
    enum_name: &Ident,
    variant_name: &Ident,
) -> TokenStream {
    let q_input_type = quote_name(input_type);

    return quote! {
        pub fn #q_input_type(input: #q_input_type) -> #output_type<'a> {
            return Ok(#enum_name::#variant_name(input));
        }
    };
}

// quote! stuff for inside of types.
fn quote_typ(typ: &Typ) -> TokenStream {
    match typ {
        Typ::Bool => quote! { bool },
        Typ::F64 => quote! { f64 },
        Typ::I32 => quote! { i32 },
        Typ::String => quote! { &'a str },
        Typ::Unknown => quote! { Unknown },
        Typ::Undefined => quote! { Undefined },
        Typ::Ref(t) => {
            let q_t = quote_typ(t);
            quote! {
                &'a RefCell<#q_t>
            }
        }
        Typ::RustType(id) => {
            let q_type = quote_rust_type_name(id);
            return quote! {
                #q_type<'a>
            };
        }
        _ => unimplemented!(),
    }
}

// enum
fn compile_union(name: &usize, ts: &ImHashSet<Typ>) -> RustTypeEnum {
    let mut rt_enum = RustTypeEnum::new(
        quote_rust_type_name(name),
        quote_name(&("RustType".to_string() + name.to_string().as_str() + "Result")),
    );

    ts.iter().enumerate().for_each(|(i, typ)| {
        let q_typ = quote_typ(typ);
        let variant = Variant::new(quote_variant_name(&i), q_typ);
        rt_enum.variants.push(variant.clone());
        match typ {
            Typ::Bool => {
                rt_enum.strict_eqs.push(variant.clone());
            }
            Typ::F64 => {
                rt_enum.strict_eqs.push(variant.clone());
                rt_enum.muls.push(variant.clone());
            }
            Typ::I32 => {
                rt_enum.strict_eqs.push(variant.clone());
                rt_enum.muls.push(variant.clone());
            }
            Typ::String => {}
            Typ::Ref(_) => {}
            _ => unimplemented!(),
        }
    });

    return rt_enum;
}

fn quote_union(rt_enum: RustTypeEnum) -> TokenStream {
    let q_name = rt_enum.name;
    let q_result_name = rt_enum.result_name;
    let mut q_variants: Vec<TokenStream> = vec![];
    let mut q_methods: Vec<TokenStream> = vec![];
    let mut q_impls: Vec<TokenStream> = vec![];

    rt_enum.variants.iter().for_each(|v| {
        let q_variant_name = v.name.to_owned();
        let q_variant_typ = v.typ.to_owned();
        q_variants.push(quote! {
            #q_variant_name(#q_variant_typ)
        });
        q_methods.push(quote! {
            pub fn #q_variant_typ(i: #q_variant_typ) -> #q_result_name<'a> {
                return Ok(#q_name::#q_variant_name(i));
            }
        });
        q_impls.push(quote! {
            impl<'a> std::convert::Into<#q_variant_typ> for #q_name<'a> {
                fn into(self) -> #q_variant_typ {
                    match self {
                        #q_name::#q_variant_name(b) => return b,
                        _ => unimplemented!()
                    }
                }
            }
        });
    });

    let q_muls: HashMap<Typ, Vec<TokenStream>> = HashMap::new();
    rt_enum.muls.iter().for_each(|v| {
        let q_variant_name = v.name.to_owned();
        let q_variant_typ = v.typ.to_owned();
    });

    return quote! {
        pub type #q_result_name<'a> = Result<#q_name<'a>,Error>;

        pub enum #q_name<'a> {
            PhantomData(PhantomData<&'a i32>),
            #(#q_variants),*
        }

        impl<'a> #q_name<'a> {
            #(#q_methods)*
        }

        #(#q_impls)*
    };
}

// struct
fn quote_object(name: &usize, tm: &ImHashMap<String, Typ>) -> TokenStream {
    let q_name = quote_rust_type_name(name);

    let q_tm = tm.iter().map(|(field, typ)| {
        let q_field = Ident::new(&format!("{}", field), Span::call_site());
        let q_typ = quote_typ(typ);
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
pub fn quote_typs(types: &HashMap<usize, Typ>) -> TokenStream {
    // Only create structs and enums for Unions and Objects.
    let q_typs = types
        .iter()
        .fold(vec![], |mut acc: Vec<TokenStream>, (name, typ)| {
            match typ {
                Typ::Union(ts) => {
                    let rt_enum = compile_union(name, ts);
                    acc.push(quote_union(rt_enum));
                    return acc;
                }
                Typ::Object(tm) => {
                    //acc.push(quote_object(name, tm));
                    return acc;
                }
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

// http://smallcultfollowing.com/babysteps/blog/2014/09/30/multi-and-conditional-dispatch-in-traits/

#[cfg(test)]
mod tests {

    use std::{collections::HashMap, io::Write};

    use crate::{
        gen::gen_types::quote_typs,
        types::{constructors::*, Typ},
    };

    fn test_harness(types: &HashMap<usize, Typ>) {
        let q_types = quote_typs(types);

        let filename = "TYPES.rs";

        let mut rs_file = std::fs::File::create(filename).expect("Could not create .rs file.");
        rs_file
            .write_all(format!("{}", q_types).as_bytes())
            .expect("Could not write to file.");
        cmd!("rustfmt", filename).run().expect("rustfmt failed");
        cmd!("rustc", "--crate-type", "cdylib", filename)
            .run()
            .expect("Compiling to Rust failed.");
    }

    #[allow(unused)]
    fn types_1() {
        let mut types = HashMap::new();
        types.insert(0, t_union_2(&[Typ::F64, Typ::Bool]));
        types.insert(1, t_union_2(&[Typ::F64, Typ::I32]));

        test_harness(&types);

        // assert!(false);
    }

}
