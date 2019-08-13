use crate::verif::untyped_traces::{Exp, Typ, Op2};
use Exp::*;
use im_rc::{HashMap as ImHashMap};

type TypEnv = ImHashMap<String, Typ>;
type Subst = std::collections::HashMap<usize, Typ>;

enum Constraint {
    /**
     * In `UnionMem(t1, t2)`, the type `t2` must be a union and `t1` is an element
     * of the union.
     */
    UnionMem(Typ, Typ)
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
            Exp::Number { value } => Ok(Typ::F64),
            Exp::Integer { value } => Ok(Typ::I32),
            Exp::Identifier { name } => env.get(name)
                .cloned()
                .ok_or_else(|| err(format!("free variable: {}", &name))),
            Exp::Stringg { value } => Ok(Typ::String),
            Exp::Ref { e } => {
                let t = self.exp(env, e)?;
                let x = self.fresh_var();
                self.constraints.push(Constraint::UnionMem(t, x.clone()));
                Ok(Typ::Ref(Box::new(x)))
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
                    format!("in SetRef(e1, e2), type of e1 is {:?}", t)))
            },
            // This should never occur. If it does, we should think about why.
            Exp::Let { name, named, typ } =>
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
                let t1 = self.exp(env, cond)?;
                let t2 = self.exp_list(env, true_part)?;
                let t3 = self.exp_list(env, false_part)?;
                let x = self.fresh_var();
                self.constraints.push(Constraint::UnionMem(t2, x.clone()));
                self.constraints.push(Constraint::UnionMem(t3, x.clone()));
                Ok(x)
            },
            Exp::From { exp, field } => {
                let t = self.exp(env, exp)?;
                // match t {
                //     Typ::Object(typ_vec) => {
                //     }
                // }
                unimplemented!()
            },
            Exp::Callback { event, event_arg, callback_args, callback_clos,
                body } => {
                // Type-check event_arg
                // The type of event tells us what the types of callback_args
                // will be.
                // 
                unimplemented!()
            },
            // Exp::Index { e1, e2 } => {
            //     let t1 = self.exp(env, e1)?;
            //     let t2 = self.exp(env, e2)?;

            // },
            _ => panic!(format!("{:?}", exp))
        }
    }

    fn subst_metavars(subst: &Subst, exp: &mut Exp) {
        match exp {
            Exp::Unknown { } => (),
            Exp::Number { value } => (),
            Exp::Identifier { name } => (),
            Exp::Stringg { value } => (),
            Exp::Ref { e } => Typeinf::subst_metavars(subst, e),
            Exp::SetRef { e1, e2 } => {
                Typeinf::subst_metavars(subst, e1);
                Typeinf::subst_metavars(subst, e2);
            },
            Exp::Let { name, named, typ } => {
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
            _ => unimplemented!()
        }

    }

    fn solve(constraints: Vec<Constraint>) -> Subst {
        use std::collections::HashMap;
        let mut subst: HashMap::<usize, Typ> = HashMap::new();
        for constraint in constraints.into_iter() {
            match constraint {
                Constraint::UnionMem(t, Typ::Metavar(n)) => {
                    let mut t1 = t.clone();
                    t1.apply_subst(&subst);
                    // Typeinf::apply_subst(&mut subst, &mut t);
                    match subst.get_mut(&n)  {
                        None => {
                            let _ = subst.insert(n, t1);
                        },
                        Some(s) => *s = Typ::Union(Box::new(s.clone()), Box::new(t1))
                    }
                },
                Constraint::UnionMem(_, _) => panic!("unionmem")
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

    use super::typeinf;
    use super::super::untyped_traces::constructors::*;
    use super::super::untyped_traces::{Exp, Typ};

    #[test]
    fn union1() {
        let mut e = block(vec![
            let_("x", ref_(number(1.0))),
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
