use super::super::types::{Exp, Op2};
use proc_macro2::Span;
use quote::__rt::TokenStream;
use quote::*;
use syn::{Ident, Lifetime};

fn codegen_op(op: &Op2) -> TokenStream {
    match op {
        Op2::Add => quote! { add },
        Op2::Sub => quote! { sub },
        Op2::GT => quote! { gt },
        Op2::StrictEq => quote! { strict_eq },
    }
}

fn codegen_block(block: &[Exp]) -> TokenStream {
    let q_block = block.iter().map(|e| codegen_exp(e));
    match block.last() {
        Some(Exp::Let { name: _, named: _, typ:_  }) => {
            quote! {
                #(#q_block)*
                Dyn::undefined()
            }
        },
        _ => {
            quote! {
                #(#q_block)*
            }
        }
    }
}

fn codegen_exp(exp: &Exp) -> TokenStream {
    match exp {
        Exp::Unknown {} => quote! { rt::unknown() },
        Exp::Integer { value } => quote! { Dyn::int(#value) },
        Exp::Number { value } => quote! { Dyn::float(#value) },
        Exp::Identifier { name } => {
            // NOTE(arjun): Writing quote! { #x } would treat x as a string and
            // generate the Rust code "x". This code is based on the following
            // example:
            // https://docs.rs/quote/0.6.12/quote/macro.quote.html#constructing-identifiers
            let q_id = Ident::new(&format!("{}", name), Span::call_site());
            quote! { #q_id }
        }
        Exp::From { exp, field } => {
            let q_exp = codegen_exp(exp);
            quote! { #q_exp.get(#field) }
        }
        Exp::Stringg { value } => quote! { Dyn::str(#value) },
        Exp::Undefined {} => quote! { Dyn::undef() },
        Exp::BinOp { op, e1, e2 } => {
            let q_op = codegen_op(op);
            let q_e1 = codegen_exp(e1);
            let q_e2 = codegen_exp(e2);
            quote! { #q_e1.#q_op(#q_e2)? }
        }
        Exp::If {
            cond,
            true_part,
            false_part,
        } => {
            let q_test = codegen_exp(cond);
            let q_then_part = codegen_block(true_part);
            let q_else_part = codegen_block(false_part);
            quote! {
                if #q_test.into() { #q_then_part } else { #q_else_part }
            }
        }
        Exp::While { cond, body } => {
            let q_cond = codegen_exp(cond);
            let q_body = codegen_exp(body);
            quote! {
                while #q_cond {
                    #q_body
                }
            }
        }
        Exp::Let {
            name,
            typ: _,
            named,
        } => {
            let q_name = Ident::new(&format!("{}", name), Span::call_site());
            let q_named = codegen_exp(named);
            quote! {
                let #q_name = #q_named;
            }
        }
        Exp::Set { name: _, named: _ } =>
        // NOTE(arjun): This should have been turned into SetRef.
        {
            panic!("unexpected Exp::Set during code generation")
        }
        Exp::Block { body } => {
            let q_body = codegen_block(body);
            quote! {
                {
                    #q_body
                }
            }
        }
        Exp::Callback {
            event: _,
            event_arg: _,
            callback_args: _,
            callback_clos: _,
            body: _,
        } =>
        // NOTE(arjun): This should have been turned into Loopback.
        {
            panic!("unexpected Exp::Callback during code generation")
        }
        Exp::Loopback {
            event,
            event_arg,
            callback_clos,
            id,
        } => {
            let q_event_arg = codegen_exp(event_arg);
            let q_callback_clos = codegen_exp(callback_clos);
            quote! {
                ec.loopback(#event, #q_event_arg, #q_callback_clos, #id)?
            }
        }
        Exp::Label { name, body } => {
            let q_body = codegen_block(body);
            let q_name = Lifetime::new(&format!("{}", name), Span::call_site());
            quote! {
                #q_name: {
                    #q_body;
                }
            }
        }
        Exp::Break { name, value } => {
            let q_name = Lifetime::new(&format!("{}", name), Span::call_site());
            let q_value = codegen_exp(value);
            quote! {
                break #q_name #q_value;
            }
        }
        Exp::Clos { tenv } => {
            let q_tenv = tenv.iter().map(|(k, v)| {
                let q_v = codegen_exp(v);
                quote! { (#k, #q_v) }
            });
            quote! { Dyn::object(arena, vec![#(#q_tenv),*]) }
        }
        Exp::Array { exps } => {
            let q_exps = exps.iter().map(|e| codegen_exp(e));
            quote! { Dyn::array(#(#q_exps),*) }
        }
        Exp::Index { e1, e2 } => {
            let q_e1 = codegen_exp(e1);
            let q_e2 = codegen_exp(e2);
            quote! { #q_e1.index(#q_e2)? }
        }
        Exp::Ref { e } => {
            let q_e = codegen_exp(e);
            quote! { Dyn::ref_(arena, #q_e) }
        }
        Exp::Deref { e } => {
            let q_e = codegen_exp(e);
            quote! { Dyn::deref(&#q_e) }
        }
        Exp::SetRef { e1, e2 } => {
            let q_e1 = codegen_exp(e1);
            let q_e2 = codegen_exp(e2);
            quote! { Dyn::setref(#q_e1, #q_e2) }
        }
        Exp::PrimApp { event, event_args } => {
            let q_event_args = event_args.iter().map(|e| codegen_exp(e));
            let q_event = Ident::new(&format!("{}", event), Span::call_site());
            quote! {
                rts.#q_event(#(#q_event_args),*);
            }
        }
    }
}

pub fn codegen(e: &Exp) -> String {
    let q_e = codegen_exp(e);
    let tokens = quote! {
        use trace_runtime::{Error, ExecutionContext, Dyn};
        use trace_runtime as rt;

        #[no_mangle]
        pub extern "C" fn containerless<'a>(
            ec: &mut ExecutionContext,
            arena: &'a bumpalo::Bump,
            arg_cbid: &'a Dyn<'a>,
            arg_cbargs: &'a Dyn<'a>) -> Result<Dyn<'a>, Error> {
            #q_e
        }
    };
    return format!("{}", tokens);
}
