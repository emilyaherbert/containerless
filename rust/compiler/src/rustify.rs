/// The Rust type-checker is pendantic in ways that benefit hand-written code,
/// but get in the way of generated code. Most of the pedantry manifests as
/// warnings, which are merely irritating. However, some pedantry manifests
/// as type errors that we have to address. This module handles both.
use super::types::Exp;

pub struct Rustify {}

impl Rustify {
    pub fn new() -> Self {
        Rustify {}
    }

    pub fn rustify_block(&mut self, block: &mut Vec<Exp>) {
        let len = block.len();
        for (ix, e) in block.iter_mut().enumerate() {
            self.rustify(e);
            if ix < len - 1 {
                if let Exp::Let {
                    name:_,
                    typ:_,
                    named:_
                } = e {
                    // empty body
                } else {
                    let inner = std::mem::replace(e, Exp::Undefined {});
                    *e = Exp::Let {
                        name: "_".to_string(),
                        typ: None,
                        named: Box::new(inner),
                    };
                }

                // We treat `if` as expressions in our IR. Conveniently, `if`
                // in Rust is an expression too! However, when an `if` is in
                // a block and *not* the last expression in the block, Rust
                // requires both branches of the `if` to return `()`. This is
                // a hack that transforms `if ...` into `let _ = if ...`, which
                // is enough to satisfy the Rust type checker.
                /*
                if let Exp::If {
                    cond: _,
                    true_part: _,
                    false_part: _,
                } = e
                {
                    let inner = std::mem::replace(e, Exp::Undefined {});
                    *e = Exp::Let {
                        name: "_".to_string(),
                        typ: None,
                        named: Box::new(inner),
                    };
                } else if let Exp::Label {
                    name,
                    body: _
                } = e
                {
                    let inner = std::mem::replace(e, Exp::Undefined {});
                    *e = Exp::Let {
                        name: "l".to_string(),
                        typ: None,
                        named: Box::new(inner)
                    };
                }
                */
            } else {
                if let Exp::Label {
                    name,
                    body: _
                } = e
                {
                    let inner = std::mem::replace(e, Exp::Undefined {});
                    *e = Exp::Block { body: vec![
                        Exp::Let {
                            name: "l".to_string(),
                            typ: None,
                            named: Box::new(inner)
                        },
                        Exp::Identifier { name: "l".to_string() }
                    ]};
                }
            }
        }
    }

    pub fn rustify(&mut self, exp: &mut Exp) {
        match exp {
            Exp::Unknown {} => (),
            Exp::Integer { value: _ } => (),
            Exp::Number { value: _ } => (),
            Exp::Bool { value: _ } => (),
            Exp::Stringg { value: _ } => (),
            Exp::Undefined {} => (),
            Exp::Unit {} => (),
            Exp::Identifier { name: _ } => (),
            Exp::From { exp, field: _ } => self.rustify(exp),
            Exp::Get { exp, field: _ } => self.rustify(exp),
            Exp::Op1 { op: _, e } => self.rustify(e),
            Exp::BinOp { op: _, e1, e2 } => {
                self.rustify(e1);
                self.rustify(e2);
            }
            Exp::If {
                cond,
                true_part,
                false_part,
            } => {
                self.rustify(cond);
                self.rustify_block(true_part);
                self.rustify_block(false_part);
            }
            Exp::While { cond, body } => {
                self.rustify(cond);
                self.rustify_block(body);
                let inner = std::mem::replace(&mut *exp, Exp::Undefined {});
                *exp = Exp::Block { body: vec! [
                    inner,
                    Exp::Undefined {}
                ]};
            }
            Exp::Let {
                name: _,
                typ: _,
                named,
            } => self.rustify(named),
            Exp::Set { name: _, named } => self.rustify(named),
            Exp::Block { body } => self.rustify_block(body),
            Exp::Callback {
                event: _,
                event_arg: _,
                callback_args: _,
                callback_clos: _,
                body: _,
            } => panic!("Exp::Callback should be eliminated"),
            Exp::Loopback {
                event: _,
                event_arg,
                callback_clos,
                id: _,
            } => {
                self.rustify(event_arg);
                self.rustify(callback_clos)
            }
            Exp::Label { name: _, body } => self.rustify_block(body),
            // Generating `break 'a break 'b e` produces an unreachable code
            // warning. This simplifies it to `break 'b e`.
            Exp::Break { name:_, value } => match **value {
                Exp::Break { name: _, value: _ } => {
                    let inner = std::mem::replace(&mut **value, Exp::Unit {});
                    *exp = inner;
                }
                _ => self.rustify(value),
            },
            Exp::Object { properties } => properties.values_mut().for_each(|e| self.rustify(e)),
            Exp::Clos { tenv: _ } => panic!("Exp::Clos should be eliminated"),
            Exp::Array { exps } => exps.iter_mut().for_each(|e| self.rustify(e)),
            Exp::Index { e1, e2 } => {
                self.rustify(e1);
                self.rustify(e2);
            },
            Exp::Ref { e } => self.rustify(e),
            Exp::Deref { e } => self.rustify(e),
            Exp::SetRef { e1, e2 } => {
                self.rustify(e1);
                self.rustify(e2);
            },
            Exp::PrimApp {
                event: _,
                event_args,
            } => event_args.iter_mut().for_each(|e| self.rustify(e)),
            Exp::MethodCall {
                e,
                method:_,
                method_call_args
            } => {
                self.rustify(e);
                method_call_args.iter_mut().for_each(|e| self.rustify(e));
            }
        }
    }
}
