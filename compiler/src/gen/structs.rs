use quote::{
    __rt::TokenStream,
    *
};

use std::collections::HashMap;

use crate::types::Typ;

// https://github.com/plasma-umass/decontainerization/blob/65d9ee78dc720efac7b6d944d8f70f608024ba2c/native/src/lib.rs#L102
pub fn compile_structs(types: &HashMap<usize, Typ>) -> TokenStream {
    let just_types: Vec<Typ> = types.iter().map(|(_, v)| v.to_owned()).collect();

    quote! {
        #[no_mangle]
        #[allow(unused_parens)]
        
        pub struct Unknown;
        pub struct Undefined;
        pub struct Ref<T>(T);
        pub struct ResponseCallback;

    }
}