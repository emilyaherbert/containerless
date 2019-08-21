use quote::{
    __rt::TokenStream,
    *
};
use syn::{
    Ident
};
use proc_macro2::Span;
use std::collections::HashMap;

use crate::types::Typ;

// quote! stuff for inside of types.
fn compile_typ(typ: &Typ) -> TokenStream {
    match typ {
        Typ::Bool => quote! { bool },
        Typ::F64 => quote! { f64 },
        Typ::I32 => quote! { i32 },
        Typ::String => quote! { String },
        Typ::Unknown => quote! { Unknown },
        Typ::Undefined => quote! { Undefined },
        Typ::Ref(t) => {
            let q_t = compile_typ(t);
            quote! {
                Ref<#q_t>
            }
        }
        Typ::RustType(id) => {
            let q_type = Ident::new(&format!("{}", "RustType".to_string() + id.to_string().as_str()), Span::call_site());
            return quote! {
                #q_type
            }
        }
        _ => panic!("Did not expect to see this here.")
    }
}

// In a variant you need to wrap primitives.
fn compile_variant(typ: &Typ) -> TokenStream {
    match typ {
        Typ::Bool => quote! { Bool(bool) },
        Typ::F64 => quote! { F64(f64) },
        Typ::I32 => quote! { I32(i32) },
        Typ::String => quote! { Stringg(String) },
        _ => compile_typ(typ)
    }
}

fn compile_field(name: &str, typ: &Typ) -> TokenStream {
    let q_name = Ident::new(&format!("{}", name), Span::call_site());
    let q_typ = compile_typ(typ);

    return quote! {
        #q_name : #q_typ
    };
}

// https://github.com/plasma-umass/decontainerization/blob/65d9ee78dc720efac7b6d944d8f70f608024ba2c/native/src/lib.rs#L102
pub fn compile_typs(types: &HashMap<usize, Typ>) -> TokenStream {

    // Only create structs and enums for Unions and Objects.
    let q_typs = types.iter()
        .fold(vec![], |mut acc: Vec<TokenStream>, (name, typ)| {
            let q_name = Ident::new(&format!("{}", "RustType".to_string() + name.to_string().as_str()), Span::call_site());
            match typ {
                Typ::Union(ts) => {
                    let q_variants = ts.iter().map(|t| compile_variant(t));
                    acc.push(quote! {
                        pub enum #q_name {
                            #(#q_variants),*
                        }
                    });
                    return acc;
                },
                Typ::Object(tm) => {
                    let q_fields = tm.iter().map(|(name, t)| compile_field(name, t));
                    acc.push(quote! {
                        pub struct #q_name {
                            #(#q_fields),*
                        }
                    });
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
        
        pub struct Unknown;
        pub struct Undefined;
        pub struct Ref<T>(T);
        pub struct ResponseCallback;

        #(#q_typs)*

    };
}