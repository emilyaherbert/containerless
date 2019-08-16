use crate::types::{Exp, Typ, Op2, Arg};
use im_rc::{HashMap as ImHashMap};

type TypEnv = ImHashMap<String, Typ>;
type Subst = std::collections::HashMap<usize, Typ>;

enum Constraint {
    /**
     * In `UnionMem(t1, t2)`, the type `t2` must be a union and `t1` is an element
     * of the union.
     */
    UnionMem(Typ, Typ),
    /**
     * In `FromMem(t1, x, t2)` the type of `t1.x = t2`.
     */
    FromMem(Typ, String, Typ),
    Eq(Typ, Typ)
}

pub struct Typeinf {
    constraints: Vec<Constraint>,
    next_var: usize
}

#[derive(Debug)]
pub struct Error {
    pub message: String
}

fn err<S>(message: S) -> Error
  where S : Into<String> {
    Error {
        message: message.into()
    }
}

impl std::fmt::Display for Error {

    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        return fmt.write_str(&self.message);
    }

}

impl std::error::Error for Error { }

fn tref(t: Typ) -> Typ {
    return Typ::Ref(Box::new(t));
}

impl Typeinf {

    fn fresh_var(&mut self) -> Typ {
        let x = self.next_var;
        self.next_var = self.next_var + 1;
        Typ::Metavar(x)
    }

    pub fn exp_list(&mut self, env: &TypEnv, exps: &mut [Exp]) -> Result<Typ, Error> {
        let mut env = env.clone();
        let mut last_typ = Typ::Undefined;
        for exp in exps.iter_mut() {
            match exp {
                Exp::Let { name, named, typ } => {
                    let t = self.exp(&env, named)?;
                    *typ = Some(t.clone());
                    env.insert(name.to_string(), t);
                    last_typ = Typ::Undefined;
                },
                _ => {
                    last_typ = self.exp(&env, exp)?;
                }
            }
        }
        Ok(last_typ)
    }


    pub fn exp(&mut self, env: &TypEnv, exp: &mut Exp) -> Result<Typ, Error> {
        match exp {
            Exp::Unknown { } => Ok(Typ::Unknown),
            Exp::Number { value:_ } => Ok(Typ::F64),
            Exp::Integer { value:_ } => Ok(Typ::I32),
            Exp::Identifier { name } => env.get(name)
                .cloned()
                .ok_or_else(|| err(format!("free variable: {}", &name))),
            Exp::Stringg { value:_ } => Ok(Typ::String),
            Exp::Ref { e } => {
                let t = self.exp(env, e)?;
                let x = self.fresh_var();
                self.constraints.push(Constraint::UnionMem(t, x.clone()));
                Ok(Typ::Ref(Box::new(x)))
            },
            Exp::Deref { e } => {
                let t = self.exp(env, e)?;
                let x = self.fresh_var();
                self.constraints.push(Constraint::Eq(tref(x.clone()), t));
                Ok(x)
            },
            // Note that there *is a possibility of a type error below, if in
            // SetRef(e1, e2), the type of e1 is not a reference type. However,
            // reference-manipulation expressions do not appear in the original
            // program and are instead added by the trace compiler, and they
            // are guaranteed to be inserted in the right place. A type error
            // indicates a bug in an earlier phase.
            Exp::SetRef { e1, e2 } => match self.exp(env, e1)? {
                Typ::Ref(x) => {
                    let t = self.exp(env, e2)?;
                    self.constraints.push(Constraint::UnionMem(t, *x.clone()));
                    Ok(Typ::Ref(x))
                },
                t => Err(err(
                    format!("in SetRef(e1, e2), type of e1 ({:?}) is {:?}", e1, t)))
            },
            // This should never occur. If it does, we should think about why.
            Exp::Let { name:_, named:_, typ:_ } =>
                Err(err("bare let expression outside a block")),
            Exp::Block { body } => self.exp_list(env, body),
            Exp::BinOp { op, e1, e2 } => {
                let t1 = self.exp(env, e1)?;
                let t2 = self.exp(env, e2)?;
                match op {
                   Op2::Add => {
                        let x = self.fresh_var();
                        self.constraints.push(Constraint::UnionMem(t1, x.clone()));
                        self.constraints.push(Constraint::UnionMem(t2, x.clone()));
                        Ok(x)
                   },
                   Op2::StrictEq => Ok(Typ::Bool),
                   _ => panic!(format!("not implemented: {:?}", op))
                }
            },
            Exp::Clos { tenv } => {
                let mut ts = Vec::new();
                for (x, e) in tenv.iter_mut() {
                    let t = self.exp(env, e)?;
                    ts.push((x.to_string(), t));
                }
                Ok(Typ::Object(ts))
            },
            Exp::If { cond, true_part, false_part } => {
                self.exp(env, cond)?;
                let t2 = self.exp_list(env, true_part)?;
                let t3 = self.exp_list(env, false_part)?;
                let x = self.fresh_var();
                self.constraints.push(Constraint::UnionMem(t2, x.clone()));
                self.constraints.push(Constraint::UnionMem(t3, x.clone()));
                Ok(x)
            },
            Exp::From { exp, field } => {
                let t = self.exp(env, exp)?;
                let x = self.fresh_var();
                self.constraints.push(Constraint::FromMem(t, field.to_string(), x.clone()));
                return Ok(x);
            },
            Exp::Callback { event:_, event_arg, callback_args, callback_clos, body } => {
                self.exp(env, event_arg)?;
                let t2 = self.exp(env, callback_clos)?;
                let mut env = env.clone();
                for Arg { name, typ } in callback_args.iter_mut() {
                    let t = match name.as_ref() {
                        "$clos" => tref(t2.clone()),
                        "$request" => tref(Typ::Object(vec![("path".to_string(), tref(Typ::String))])),
                        "$responseCallback" => tref(Typ::ResponseCallback),
                        "$response" => tref(Typ::Union(vec![Typ::String, Typ::Undefined])),
                        _ => panic!("Unexpected argument!")
                    };
                    *typ = Some(t.clone());
                    env.insert(name.to_string(), t);
                }
                let t3 = self.exp_list(&env, body)?;
                Ok(t3)
            },
            Exp::Label { name:_, body } => {
                self.exp_list(env, body)
            },
            Exp::PrimApp { event: _, event_args } => {
                self.exp_list(&env, event_args)?;
                Ok(Typ::Undefined)
            },
            _ => panic!(format!("{:?}", exp))
        } 
    }

    fn subst_metavars(subst: &Subst, exp: &mut Exp) {
        match exp {
            Exp::Unknown { } => (),
            Exp::Number { value:_ } => (),
            Exp::Identifier { name:_ } => (),
            Exp::Stringg { value:_ } => (),
            Exp::Ref { e } => Typeinf::subst_metavars(subst, e),
            Exp::Deref { e } => Typeinf::subst_metavars(subst, e),
            Exp::SetRef { e1, e2 } => {
                Typeinf::subst_metavars(subst, e1);
                Typeinf::subst_metavars(subst, e2);
            },
            Exp::Let { name:_, named, typ } => {
                Typeinf::subst_metavars(subst, named);
                match typ {
                    Some(t) => t.apply_subst(subst),
                    None => ()
                }
            },
            Exp::Block { body } => {
                for e in body.iter_mut() {
                    Typeinf::subst_metavars(subst, e)
                }
            },
            Exp::Clos { tenv } => {
                for (_, e) in tenv.iter_mut() {
                    Typeinf::subst_metavars(subst, e);
                }
            },
            Exp::Callback { event:_, event_arg, callback_args, callback_clos, body } => {
                Typeinf::subst_metavars(subst, event_arg);
                Typeinf::subst_metavars(subst, callback_clos);
                for Arg { name:_, typ } in callback_args.iter_mut() {
                    match typ {
                        Some(t) => t.apply_subst(subst),
                        None => ()
                    }
                }
                for e in body.iter_mut() {
                    Typeinf::subst_metavars(subst, e);
                }
            },
            Exp::Label { name:_, body } => {
                for e in body.iter_mut() {
                    Typeinf::subst_metavars(subst, e);
                }
            },
            Exp::PrimApp { event:_, event_args } => {
                for e in event_args.iter_mut() {
                    Typeinf::subst_metavars(subst, e);
                }
            },
            Exp::From { exp, field:_ } => {
                Typeinf::subst_metavars(subst, exp);
            },
            Exp::If { cond, true_part, false_part } => {
                Typeinf::subst_metavars(subst, cond);
                for e in true_part.iter_mut() {
                    Typeinf::subst_metavars(subst, e);
                }
                for e in false_part.iter_mut() {
                    Typeinf::subst_metavars(subst, e);
                }
            },
            Exp::BinOp { op:_, e1, e2 } => {
                Typeinf::subst_metavars(subst, e1);
                Typeinf::subst_metavars(subst, e2);
            }
            _ => unimplemented!("{:?}", exp),
        }
    }

    fn unify(subst: &mut std::collections::HashMap::<usize, Typ>,
        t1: &Typ, t2: &Typ) {
        match (t1, t2) {
            (Typ::Metavar(x), _) => {
                subst.insert(*x, t2.clone());
            },
            (_, Typ::Metavar(y)) => {
                subst.insert(*y, t1.clone());
            },
            (Typ::Bool, Typ::Bool) => (),
            (Typ::F64, Typ::F64) => (),
            (Typ::Ref(t1), Typ::Ref(t2)) => Typeinf::unify(subst, t1, t2),
            _ => {
                panic!(format!("Cannot unify {:?} with {:?}", t1, t2))
            }
        }
    }

    fn solve(constraints: Vec<Constraint>) -> Subst {
        use std::collections::HashMap;
        let mut subst: HashMap::<usize, Typ> = HashMap::new();
        for constraint in constraints.into_iter() {
            match constraint {
                Constraint::Eq(t1, t2) => {
                    let mut t1 = t1.clone();
                    t1.apply_subst(&subst);
                    let mut t2 = t2.clone();
                    t2.apply_subst(&subst);
                    Typeinf::unify(&mut subst, &t1, &t2);
                },
                Constraint::UnionMem(t, Typ::Metavar(n)) => {
                    let mut t1 = t.clone();
                    t1.apply_subst(&subst);
                    match subst.get_mut(&n) {
                        None => {
                            let _ = subst.insert(n, t1);
                        },
                        Some(Typ::Union(ts1)) => {
                            match t1 {
                                Typ::Union(ts2) => {
                                    for ts3 in ts2.into_iter() {
                                        if !ts1.contains(&ts3) {
                                            ts1.push(ts3);
                                        }
                                    }
                                },
                                ts2 => {
                                    if !ts1.contains(&ts2) {
                                        ts1.push(ts2);
                                    }
                                }
                            }
                        }
                        Some(s) => {
                            *s = Typ::Union(vec![s.clone(), t1]);
                        }
                    }
                },
                Constraint::UnionMem(_, _) => panic!("unionmem"),
                Constraint::FromMem(t, field, Typ::Metavar(n)) => {
                    let mut t1 = t.clone();
                    t1.apply_subst(&subst);

                    match t1 {
                        Typ::Object(typ_vec) => {
                            // https://www.reddit.com/r/rust/comments/7mqwjn/hashmapstringt_vs_vecstringt/
                            match typ_vec.iter().find(|x|*x.0 == field) {
                                Some((_k, v)) => {
                                    match subst.get_mut(&n) {
                                        None => {
                                            let _ = subst.insert(n, v.clone());
                                        },
                                        Some(Typ::Union(ts)) => {
                                            ts.push(v.clone());
                                        }
                                        Some(s) => {
                                            *s = Typ::Union(vec![s.clone(), v.clone()]);
                                        }
                                    }
                                }
                                None => {
                                    panic!("Not found {:?} ----> {:?}", typ_vec, field)
                                }
                            }
                        },
                        _ =>
                            panic!(format!("FromMem({:?}, {:?}, Metavar({:?}))", t1, field, n))
                    }
                },
                Constraint::FromMem(_, _, _) => panic!("frommem"),
            }
        }
        let mut at_fixed_point = true;
        loop {
            let mut new_subst: HashMap::<usize, Typ> = HashMap::new();
            for (x, t) in subst.iter() {
                let mut t1 = t.clone();
                if t.has_metavars() {
                    t1.apply_subst(&subst);
                    at_fixed_point = false;
                }
                new_subst.insert(*x, t1);
            }
            subst = new_subst;
            if at_fixed_point {
                break;
            } else {
                at_fixed_point = true;
            }
        }
        // Iterate over the substitution, applying it to types that have
        // metavariables until we reach a fixed point
        // Also have an occurs check.
        return subst;
    }

}

pub fn typeinf(exp: &mut Exp) -> Result<(), Error> {
    let mut state  = Typeinf { constraints: vec![], next_var: 0 };
    let env = ImHashMap::new();
    state.exp(&env, exp)?;
    let subst = Typeinf::solve(state.constraints);
    Typeinf::subst_metavars(&subst, exp);
    Ok(())
}

#[cfg(test)]
mod tests {

    use crate::{
        types::{
            Exp,
            Typ,
            constructors::*
        },
        verif::typeinf::typeinf
    };

    #[test]
    fn union1() {
        let mut e = block(vec![
            let_("x", None, ref_(number(1.0))),
            setref(id("x"), string("hi"))
        ]);
        typeinf(&mut e).unwrap();
        match e {
            Exp::Block { body } => match &body[..] {
                [ Exp::Let { name, named, typ: Some(t) }, _ ] => {
                    assert!(*t == t_ref(t_union(Typ::F64, Typ::String)));
                },
                _ => panic!("wrong shape")
            },
            _ => panic!("wrong shape")
        }
    }
}

/*


let o = { };

if (...) {
     o.x = "900";
     }

else {
    o.y = true;
}

typeinf(x * x) = (num, [x -> num])

typeinf(let y = x * x)

let x = "number";
let y = {
    let x = 20;
    50;
}

let x = "number";
let y = x * x;

typeinf([ x -> str ], let y = x * x, _)

typeinf([ x -> str ], x * x, _)

typeinf([ x -> str ], x, number)

let x = "foo";
let y = x + x;
let z = y + y;
let w = z + " hi";

let o1 = { };
o1.x = 20;
o1.y = 30;
o1.z = 50;




*/
