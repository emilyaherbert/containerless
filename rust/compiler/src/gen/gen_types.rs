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

// quote! stuff for inside of types.
fn compile_typ(typ: &Typ) -> TokenStream {
    match typ {
        Typ::Bool => {
            let t = compile_name("Bool");
            quote! { #t }
        },
        Typ::F64 => {
            let t = compile_name("F64");
            quote! { #t }
        },
        Typ::I32 => {
            let t = compile_name("I32");
            quote! { #t }
        },
        Typ::String => {
            let t = compile_name("Str");
            quote! { #t<'a> }
        },
        Typ::Unknown => {
            let t = compile_name("Unknown");
            quote! { #t }
        },
        Typ::Undefined => {
            let t = compile_name("Undefined");
            quote! { #t }
        },
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

fn compile_union(name: &usize, ts: &ImHashSet<Typ>) -> TokenStream {
    let q_name = compile_rust_type_name(name);
    let q_result_name = Ident::new(&format!("{}", "RustType".to_string() + name.to_string().as_str() + "Result"), Span::call_site());
    let q_result_type = quote! {
        pub type #q_result_name<'a> = Result<#q_name<'a>,Error>;
    };
    let mut q_methods: Vec<TokenStream> = vec![];

    let q_ts = ts.iter().enumerate().map(|(i,typ)| {
        let q_variant_name = compile_variant_name(&i);
        let q_typ = compile_typ(typ);
        match typ {
            Typ::Bool => {
                q_methods.push(quote! {
                    pub fn bool(b: bool) -> #q_result_type<'a> {
                        Ok(#q_name::#q_variant_name(b));
                    }
                });
                quote! { #q_variant_name(#q_typ) }
            },
            Typ::F64 => {
                
                quote! { #q_variant_name(#q_typ) }
            },
            Typ::I32 => quote! { #q_variant_name(#q_typ) },
            Typ::String => quote! { #q_variant_name(#q_typ) },
            Typ::Ref(_) => quote! { #q_variant_name(#q_typ) },
            _ => q_typ
        }
    });

    return quote! {
        pub enum #q_name<'a> {
            PhantomData(PhantomData<&'a i32>),
            #(#q_ts),*
        }

        
    };
}

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

        use std::cell::RefCell;
        use std::marker::PhantomData;

        pub struct I32(i32);

        impl std::convert::Into<i32> for I32 {
            fn into(self) -> i32 {
                return self.0;
            }
        }

        pub struct F64(f64);

        impl std::convert::Into<f64> for F64 {
            fn into(self) -> f64 {
                return self.0;
            }
        }

        pub struct Bool(bool);

        impl std::convert::Into<bool> for Bool {
            fn into(self) -> bool {
                return self.0;
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