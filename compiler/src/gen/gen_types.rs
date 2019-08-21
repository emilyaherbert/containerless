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
                &'a Cell<#q_t>
            }
        }
        Typ::RustType(id) => {
            let q_type = Ident::new(&format!("{}", "RustType".to_string() + id.to_string().as_str()), Span::call_site());
            return quote! {
                #q_type<'a>
            }
        }
        _ => unimplemented!()
    }
}

fn compile_union(name: &usize, ts: &ImHashSet<Typ>) -> TokenStream {
    let q_name = Ident::new(&format!("{}", "RustType".to_string() + name.to_string().as_str()), Span::call_site());

    let q_ts = ts.iter().map(|typ| {
        let q_typ = compile_typ(typ);
        match typ {
            Typ::Bool => quote! { Bool(#q_typ) },
            Typ::F64 => quote! { F64(#q_typ) },
            Typ::I32 => quote! { I32(#q_typ) },
            Typ::String => quote! { Str(#q_typ) },
            Typ::Ref(_) => quote! { Ref(#q_typ) },
            _ => q_typ
        }
    });

    return quote! {
        pub enum #q_name<'a> {
            #(#q_ts),*
        }
    };
}

fn compile_object(name: &usize, tm: &ImHashMap<String, Typ>) -> TokenStream {
    let q_name = Ident::new(&format!("{}", "RustType".to_string() + name.to_string().as_str()), Span::call_site());

    if tm.len() > 0 {
        let q_tm = tm.iter().map(|(field, typ)| {
            let q_field = Ident::new(&format!("{}", field), Span::call_site());
            let q_typ = compile_typ(typ);
            quote! {
                #q_field : #q_typ
            }
        });
        
        return quote! {
            pub struct #q_name<'a> {
                #(#q_tm),*
            }
        };
    } else {
        return quote! {
            pub struct #q_name { }
        };
    }
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

        use std::cell::{
            Cell
        };

        #(#q_typs)*

    };
}