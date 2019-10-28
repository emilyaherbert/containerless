use crate::types::{Exp, Op2, Op1};
use proc_macro2::Span;
use quote::__rt::TokenStream;
use quote::*;
use syn::{Ident, Lifetime};

fn codegen_op1(op: &Op1) -> TokenStream {
    match op {
        Op1::Void => quote! { void },
        Op1::Typeof => quote! { typeof_ },
        Op1::Negative => quote! { neg }
    }
}

fn codegen_op(op: &Op2) -> TokenStream {
    match op {
        Op2::Add => quote! { add },
        Op2::Sub => quote! { sub },
        Op2::Mul => quote! { mul },
        Op2::Div => quote! { div },
        Op2::StrictEq => quote! { strict_eq },
        Op2::GT => quote! { gt },
        Op2::LT => quote! { lt },
        Op2::GTE => quote! { gte },
        Op2::LTE => quote! { lte },
        Op2::And => quote! { and },
        Op2::Or => quote! { or },
        Op2::StrictNotEq => quote! { strict_neq }
    }
}

fn codegen_block(block: &[Exp], to_break: Option<Lifetime>) -> TokenStream {
    let undef = Exp::Undefined {};
    let (last, all_but_last) = match block.last() {
        Some(Exp::Let { name: _, named: _, typ: _ }) =>
          (&undef, block),
        Some(_) => block.split_last().unwrap(),
        None => (&undef, block)
    };

    let q_block_but_last = all_but_last.iter().map(|e| codegen_exp(e));
    let q_last = codegen_exp(last);
    match (to_break, last) {
        // If the last element is already a break, then don't introduce another
        // break.
        // TODO(arjun): Perhaps this transformation should happen in rustify.rs.
        (_, Exp::Break { name: _, value: _ }) => quote! {
                #(#q_block_but_last)*
                #q_last
        },
        (None, _) => {
            quote! {
                #(#q_block_but_last)*
                #q_last
            }
        },
        (Some(lifetime), _) => {
            quote! {
                #(#q_block_but_last)*
                break #lifetime #q_last;
            }
        }
    }
}

fn codegen_exp(exp: &Exp) -> TokenStream {
    match exp {
        Exp::Clos { tenv: _ } => panic!("Exp::Clos should have been eliminated"),
        Exp::Unknown {} => quote! { rt::unknown()? },
        Exp::Integer { value } => quote! { Dyn::int(#value) },
        Exp::Number { value } => quote! { Dyn::float(#value) },
        Exp::Bool { value } => quote! { Dyn::bool(#value) },
        Exp::Identifier { name } => {
            // NOTE(arjun): Writing quote! { #x } would treat x as a string and
            // generate the Rust code "x". This code is based on the following
            // example:
            // https://docs.rs/quote/0.6.12/quote/macro.quote.html#constructing-identifiers
            let q_id = Ident::new(&format!("{}", name), Span::call_site());
            quote! { #q_id }
        }
        Exp::From { exp: _, field: _ } => panic!("Exp::From should be eliminated"),
        Exp::Get { exp, field } => {
            let q_exp = codegen_exp(exp);
            quote! { #q_exp.get(#field)? }
        }
        Exp::Stringg { value } => quote! { Dyn::str(arena, #value) },
        Exp::Undefined {} => quote! { Dyn::undef() },
        Exp::Unit {} => quote! { () },
        Exp::BinOp { op, e1, e2 } => {
            let q_op = codegen_op(op);
            let q_e1 = codegen_exp(e1);
            let q_e2 = codegen_exp(e2);
            match op {
                Op2::Add => quote! { (#q_e1).#q_op(arena, #q_e2)? },
                _ => quote! { (#q_e1).#q_op(#q_e2)? }
            }
        }
        Exp::Op1 { op, e } => {
            let q_op = codegen_op1(op);
            let q_e = codegen_exp(e);
            quote! { (#q_e).#q_op(arena)? }
        }
        Exp::If {
            cond,
            true_part,
            false_part,
        } => {
            let q_test = codegen_exp(cond);
            let q_then_part = codegen_block(true_part, None);
            let q_else_part = codegen_block(false_part, None);
            quote! {
                if #q_test.into() { #q_then_part } else { #q_else_part }
            }
        }
        Exp::While { cond, body } => {
            let q_cond = codegen_exp(cond);
            let q_body = codegen_block(body, None);
            quote! {
                while (#q_cond).into() {
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
            let q_body = codegen_block(body, None);
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
            let mut q_name = Lifetime::new(&format!("{}", "'a"), Span::call_site());
            match name.chars().next() {
                Some(s) => {
                    if s == '\'' {
                        q_name = Lifetime::new(&format!("{}", name), Span::call_site());
                    } else {
                        q_name = Lifetime::new(&format!("'{}", name), Span::call_site());
                    }
                },
                None => panic!("This should not happen.")
            }
            let q_body = codegen_block(body, Some(q_name.clone()));
            quote! {
                // NOTE(arjun): Unfortunately, labelled blocks are a nightly-only
                // feature.
                #q_name: loop {
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
        Exp::Object { properties } => {
            let q_tenv = properties.iter().map(|(k, v)| {
                let q_v = codegen_exp(v);
                quote! { (#k, #q_v) }
            });
            quote! { Dyn::object_with(arena, vec![#(#q_tenv),*]) }
        }
        Exp::Array { exps } => {
            let q_exps = exps.iter().map(|e| codegen_exp(e));
            quote! { Dyn::vec_with(arena, vec![#(#q_exps),*]) }
        }
        Exp::Index { e1, e2 } => {
            let q_e1 = codegen_exp(e1);
            let q_e2 = codegen_exp(e2);
            quote! { (#q_e1).index(arena, #q_e2)? }
        }
        Exp::Ref { e } => {
            let q_e = codegen_exp(e);
            quote! { Dyn::ref_(arena, #q_e) }
        }
        Exp::Deref { e } => {
            let q_e = codegen_exp(e);
            quote! { Dyn::deref(#q_e) }
        }
        Exp::SetRef { e1, e2 } => {
            let q_e1 = codegen_exp(e1);
            let q_e2 = codegen_exp(e2);
            quote! { #q_e1.setref(#q_e2)?; }
        }
        Exp::PrimApp { event, event_args } => {
            let q_event_args = event_args.iter().map(|e| codegen_exp(e));
            let q_event = Ident::new(&format!("{}", event), Span::call_site());
            quote! {
                ec.#q_event(#(#q_event_args),*)?
            }
        },
        Exp::MethodCall { e, method, method_call_args } => {
            let q_e = codegen_exp(e);
            let q_method_call_args = method_call_args.iter().map(|e| codegen_exp(e));
            let q_method = Ident::new(&format!("{}", method), Span::call_site());
            quote! {
                #q_e.#q_method(#(#q_method_call_args),*)?
            }
        }

    }
}

pub fn codegen(e: &Exp, dest_file: &str) {
    let q_e = codegen_exp(e);
    let tokens = quote! {
        // We generate names from JavaScript, so camelCase names are inevitable.
        #![allow(non_snake_case)]
        use invoker::trace_runtime::{self as rt, ExecutionContext, Dyn, DynResult};

        pub fn containerless<'a>(
            arena: &'a bumpalo::Bump,
            ec: &mut ExecutionContext<'a>,
            arg_cbid: Dyn<'a>,
            arg_cbargs: Dyn<'a>,
        ) -> DynResult<'a> {
            Ok(#q_e)
        }
    };

    std::fs::write(dest_file, format!("{}", tokens))
        .expect(&format!("could not write to {}", dest_file));

    // NOTE(arjun): If we ever measure compilation time, this line *must* be
    // removed.
    cmd!("rustfmt", dest_file).run()
        .expect("rustfmt failed");
}
