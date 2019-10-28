#![allow(dead_code)]
#![allow(unused_imports)]

use crate::types::{constructors::*, Arg, Exp, Op2, Typ, LVal};

use std::collections::HashMap;

use im_rc::{HashMap as ImHashMap, HashSet as ImHashSet};

type TypEnv = ImHashMap<String, Typ>;
type Subst = std::collections::HashMap<usize, Typ>;

// TODO(emily): Huge TODO, implement thing where `let x = 0; x = { y:0 }; x.y;`
// This can happen with FromMem and IndexMem.

#[derive(Debug)]
enum Constraint {
    /**
     * In `UnionMem(t1, Typ::Unionvar(u))`, the type `t1` is an element of u.
     */
    UnionMem(Typ, Typ),
    /**
     * In `FromMem(t1, foo, Typ::Metavar(x))` Typ::Metavar(x) is an alias to t1.foo.
     */
    FromMem(Typ, String, Typ),
    /**
     * In `IndexMem(t1, Typ::Metavar(x))`, where `t1` is some `Typ::Array(t2)`, Typ::Metavar(x) is an alias to t2.
     */
    IndexMem(Typ, Typ),
    Eq(Typ, Typ),
}

#[derive(Debug)]
pub struct Error {
    pub message: String,
}

fn err<S>(message: S) -> Error
where
    S: Into<String>,
{
    Error {
        message: message.into(),
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        return fmt.write_str(&self.message);
    }
}

impl std::error::Error for Error {}

pub struct Typeinf {
    constraints: Vec<Constraint>,
    next_var: usize,
}

impl Typeinf {
    fn fresh_metavar(&mut self) -> Typ {
        let x = self.next_var;
        self.next_var = self.next_var + 1;
        return Typ::Metavar(x);
    }

    fn fresh_unionvar(&mut self) -> Typ {
        let x = self.next_var;
        self.next_var = self.next_var + 1;
        return Typ::Unionvar(x);
    }

    fn lval(&mut self, env: &TypEnv, lval: &mut LVal) -> Result<Typ, Error> {
        match lval {
            LVal::Index { exp, index } => {
                let mut exp = index_(*exp.to_owned(), *index.to_owned());
                return self.exp(env, &mut exp);
            },
            _ => unimplemented!()
        }
    }

    fn exp_list(&mut self, env: &TypEnv, exps: &mut [Exp]) -> Result<Typ, Error> {
        let mut env = env.clone();
        let mut last_typ = Typ::Undefined;
        for exp in exps.iter_mut() {
            match exp {
                Exp::Let { name, named, typ } => {
                    let t = self.exp(&env, named)?;
                    *typ = Some(t.clone());
                    env.insert(name.to_string(), t);
                    last_typ = Typ::Undefined;
                }
                _ => {
                    last_typ = self.exp(&env, exp)?;
                }
            }
        }
        Ok(last_typ)
    }

    pub fn exp(&mut self, env: &TypEnv, exp: &mut Exp) -> Result<Typ, Error> {
        match exp {
            Exp::Unknown {} => Ok(Typ::Unknown),
            Exp::Number { value: _ } => Ok(Typ::F64),
            Exp::Bool { value: _ } => Ok(Typ::Bool),
            Exp::Integer { value: _ } => Ok(Typ::I32),
            Exp::Identifier { name } => env
                .get(name)
                .cloned()
                .ok_or_else(|| err(format!("free variable: {}", &name))),
            Exp::Stringg { value: _ } => Ok(Typ::String),
            Exp::Ref { e } => {
                let t = self.exp(env, e)?;
                let u = self.fresh_unionvar();
                self.constraints.push(Constraint::UnionMem(t, u.clone()));
                let x = self.fresh_metavar();
                self.constraints.push(Constraint::Eq(t_ref(u), x.clone()));
                return Ok(x);
            }
            Exp::Deref { e } => match self.exp(env, e)? {
                Typ::Metavar(x) => {
                    let y = self.fresh_metavar();
                    self.constraints
                        .push(Constraint::Eq(Typ::Metavar(x), t_ref(y.clone())));
                    return Ok(y);
                }
                t => panic!("Expected metavar but found {:?}.", t),
            },
            Exp::SetRef { e1, e2 } => match self.exp(env, e1)? {
                Typ::Metavar(x) => {
                    let t2 = self.exp(env, e2)?;
                    let y = self.fresh_metavar();
                    self.constraints.push(Constraint::Eq(Typ::Metavar(x), t_ref(y.clone())));
                    self.constraints.push(Constraint::UnionMem(t2, y.clone()));
                    return Ok(Typ::Undefined);
                }
                _ => panic!("Expected metavar here."),
            },
            Exp::Set { name, named } => {
                match self.lval(env, name)? {
                    Typ::Metavar(x) => {
                        let t2 = self.exp(env, named)?;
                        let y = self.fresh_metavar();
                        self.constraints.push(Constraint::Eq(Typ::Metavar(x), y.clone()));
                        self.constraints.push(Constraint::UnionMem(t2, y.clone()));
                        return Ok(Typ::Undefined);
                    }
                    _ => panic!("Expected metavar here.")
                }
            },
            // This should never occur. If it does, we should think about why.
            Exp::Let {
                name: _,
                named: _,
                typ: _,
            } => Err(err("bare let expression outside a block")),
            Exp::Block { body } => self.exp_list(env, body),
            Exp::BinOp { op, e1, e2 } => {
                let t1 = self.exp(env, e1)?;
                let t2 = self.exp(env, e2)?;
                match op {
                    Op2::Add => {
                        let u = self.fresh_unionvar();
                        self.constraints.push(Constraint::UnionMem(t1, u.clone()));
                        self.constraints.push(Constraint::UnionMem(t2, u.clone()));
                        Ok(u)
                    }
                    Op2::Sub => {
                        let u = self.fresh_unionvar();
                        self.constraints.push(Constraint::UnionMem(t1, u.clone()));
                        self.constraints.push(Constraint::UnionMem(t2, u.clone()));
                        Ok(u)
                    }
                    Op2::Mul => {
                        let u = self.fresh_unionvar();
                        self.constraints.push(Constraint::UnionMem(t1, u.clone()));
                        self.constraints.push(Constraint::UnionMem(t2, u.clone()));
                        Ok(u)
                    }
                    Op2::Div => {
                        let u = self.fresh_unionvar();
                        self.constraints.push(Constraint::UnionMem(t1, u.clone()));
                        self.constraints.push(Constraint::UnionMem(t2, u.clone()));
                        Ok(u)
                    }
                    Op2::StrictEq => Ok(Typ::Bool),
                    Op2::StrictNotEq => Ok(Typ::Bool),
                    Op2::GT => Ok(Typ::Bool),
                    Op2::LT => Ok(Typ::Bool),
                    Op2::GTE => Ok(Typ::Bool),
                    Op2::LTE => Ok(Typ::Bool),
                    Op2::And => Ok(Typ::Bool),
                    Op2::Or => Ok(Typ::Bool),
                }
            }
            Exp::Object { properties } => {
                let mut ts = ImHashMap::new();
                for (x, e) in properties.iter_mut() {
                    let t = self.exp(env, e)?;
                    ts.insert(x.to_string(), t);
                }
                Ok(Typ::Object(ts))
            }
            Exp::If {
                cond,
                true_part,
                false_part,
            } => {
                self.exp(env, cond)?;
                let t2 = self.exp_list(env, true_part)?;
                let t3 = self.exp_list(env, false_part)?;
                let u = self.fresh_unionvar();
                self.constraints.push(Constraint::UnionMem(t2, u.clone()));
                self.constraints.push(Constraint::UnionMem(t3, u.clone()));
                return Ok(Typ::Undefined);
            }
            Exp::From { exp, field } => {
                let t = self.exp(env, exp)?;
                let x = self.fresh_metavar();
                self.constraints.push(Constraint::FromMem(t, field.to_string(), x.clone()));
                return Ok(x);
            }
            Exp::Callback {
                event: _,
                event_arg,
                callback_args,
                callback_clos,
                body,
            } => {
                self.exp(env, event_arg)?;
                let t2 = self.exp(env, callback_clos)?;
                let mut env = env.clone();
                for Arg { name, typ } in callback_args.iter_mut() {
                    let x = self.fresh_metavar();
                    let t = match name.as_ref() {
                        "clos" => t_ref(t2.clone()),
                        "request" => {
                            let mut hm = ImHashMap::new();
                            hm.insert("path".to_string(), t_ref(Typ::String));
                            t_ref(Typ::Object(hm))
                        }
                        "response_callback" => t_ref(Typ::ResponseCallback),
                        "response" => {
                            let mut hs = ImHashSet::new();
                            hs.insert(Typ::String);
                            hs.insert(Typ::Undefined);
                            t_ref(Typ::Union(hs))
                        }
                        _ => panic!("Unexpected argument {}", name),
                    };
                    self.constraints.push(Constraint::Eq(t, x.clone()));
                    *typ = Some(x.clone());
                    env.insert(name.to_string(), x);
                }
                let t3 = self.exp_list(&env, body)?;
                Ok(t3)
            }
            Exp::Label { name: _, body } => self.exp_list(env, body),
            Exp::Break { name:_, value } => self.exp(env, value),
            Exp::PrimApp {
                event: _,
                event_args,
            } => {
                self.exp_list(env, event_args)?;
                Ok(Typ::Undefined)
            }
            Exp::While { cond, body } => {
                self.exp(env, cond)?;
                self.exp_list(env, body)?;
                return Ok(Typ::Undefined);
            }
            Exp::Array { exps } => {
                if exps.len() == 0 {
                    return Ok(t_array(Typ::Undefined));
                } else {
                    let u = self.fresh_unionvar();
                    exps.iter_mut().for_each(|e| {
                        let t = self.exp(env, e).unwrap();
                        self.constraints.push(Constraint::UnionMem(t, u.clone()));
                    });
                    let x = self.fresh_metavar();
                    self.constraints.push(Constraint::Eq(t_array(u), x.clone()));
                    return Ok(x);
                }
            }
            Exp::Index { e1, e2 } => {
                let t = self.exp(env, e1)?;
                self.exp(env, e2)?;
                let x = self.fresh_metavar();
                self.constraints.push(Constraint::IndexMem(t, x.clone()));
                return Ok(x);
            }
            _ => panic!(format!("{:?}", exp)),
        }
    }

    fn subst_vars_lval(subst: &Subst, lval: &mut LVal) {
        match lval {
            LVal::Index { exp, index } => {
                Typeinf::subst_vars(subst, exp);
                Typeinf::subst_vars(subst, index);
            },
            _ => unimplemented!()
        }
    }

    fn subst_vars(subst: &Subst, exp: &mut Exp) {
        match exp {
            Exp::Unknown {} => (),
            Exp::Number { value: _ } => (),
            Exp::Integer { value: _ } => (),
            Exp::Bool { value: _ } => (),
            Exp::Identifier { name: _ } => (),
            Exp::Stringg { value: _ } => (),
            Exp::Ref { e } => Typeinf::subst_vars(subst, e),
            Exp::Deref { e } => Typeinf::subst_vars(subst, e),
            Exp::SetRef { e1, e2 } => {
                Typeinf::subst_vars(subst, e1);
                Typeinf::subst_vars(subst, e2);
            },
            Exp::Set { name, named } => {
                Typeinf::subst_vars_lval(subst, name);
                Typeinf::subst_vars(subst, named);
            }
            Exp::Let {
                name: _,
                named,
                typ,
            } => {
                Typeinf::subst_vars(subst, named);
                match typ {
                    Some(t) => t.apply_subst_strict(subst),
                    None => (),
                }
            }
            Exp::Block { body } => {
                for e in body.iter_mut() {
                    Typeinf::subst_vars(subst, e)
                }
            }
            Exp::Object { properties } => {
                for (_, e) in properties.iter_mut() {
                    Typeinf::subst_vars(subst, e);
                }
            }
            Exp::Callback {
                event: _,
                event_arg,
                callback_args,
                callback_clos,
                body,
            } => {
                Typeinf::subst_vars(subst, event_arg);
                Typeinf::subst_vars(subst, callback_clos);
                for Arg { name: _, typ } in callback_args.iter_mut() {
                    match typ {
                        Some(t) => t.apply_subst_strict(subst),
                        None => (),
                    }
                }
                for e in body.iter_mut() {
                    Typeinf::subst_vars(subst, e);
                }
            }
            Exp::Label { name: _, body } => {
                for e in body.iter_mut() {
                    Typeinf::subst_vars(subst, e);
                }
            },
            Exp::Break { name: _, value } => {
                Typeinf::subst_vars(subst, value);
            }
            Exp::PrimApp {
                event: _,
                event_args,
            } => {
                for e in event_args.iter_mut() {
                    Typeinf::subst_vars(subst, e);
                }
            }
            Exp::From { exp, field: _ } => {
                Typeinf::subst_vars(subst, exp);
            }
            Exp::If {
                cond,
                true_part,
                false_part,
            } => {
                Typeinf::subst_vars(subst, cond);
                for e in true_part.iter_mut() {
                    Typeinf::subst_vars(subst, e);
                }
                for e in false_part.iter_mut() {
                    Typeinf::subst_vars(subst, e);
                }
            }
            Exp::BinOp { op: _, e1, e2 } => {
                Typeinf::subst_vars(subst, e1);
                Typeinf::subst_vars(subst, e2);
            }
            Exp::While { cond, body } => {
                Typeinf::subst_vars(subst, cond);
                for e in body.iter_mut() {
                    Typeinf::subst_vars(subst, e);
                }
            }
            Exp::Array { exps } => {
                for e in exps.iter_mut() {
                    Typeinf::subst_vars(subst, e);
                }
            }
            Exp::Index { e1, e2 } => {
                Typeinf::subst_vars(subst, e1);
                Typeinf::subst_vars(subst, e2);
            }
            _ => unimplemented!("{:?}", exp),
        }
    }

    fn unify(subst: &mut std::collections::HashMap<usize, Typ>, t1: &Typ, t2: &Typ) {
        match (t1, t2) {
            (Typ::Metavar(x), _) => {
                subst.insert(*x, t2.clone());
            }
            (_, Typ::Metavar(y)) => {
                subst.insert(*y, t1.clone());
            }
            (Typ::Bool, Typ::Bool) => (),
            (Typ::String, Typ::String) => (),
            (Typ::F64, Typ::F64) => (),
            (Typ::Ref(t1), Typ::Ref(t2)) => Typeinf::unify(subst, t1, t2),
            (Typ::Array(t1), Typ::Array(t2)) => Typeinf::unify(subst, t1, t2),
            (Typ::Unionvar(n), Typ::Unionvar(m)) => {
                // TODO(emily): I *think* this means that they should now be the same,
                // not that we should perform some type of unification.
                match subst.get(n) {
                    Some(t1) => match subst.get(m) {
                        Some(t2) => {
                            if t1 != t2 {
                                panic!("Found mismatched types.");
                            }
                        }
                        None => panic!("Unionvar not found."),
                    },
                    None => panic!("Unionvar not found."),
                }
            }
            _ => panic!(format!("Cannot unify {:?} with {:?}", t1, t2)),
        }
    }

    fn subst_insert(subst: &mut Subst, n: usize, typ: Typ) {
        match subst.get_mut(&n) {
            None => {
                subst.insert(n, typ);
            }
            Some(Typ::Union(typs)) => match typ {
                Typ::Union(typs2) => {
                    *typs = typs.clone().union(typs2);
                }
                typ => {
                    typs.insert(typ);
                }
            },
            Some(t) => match typ {
                Typ::Union(typs) => {
                    let mut typs2 = typs.clone();
                    typs2.insert(t.clone());
                    *t = Typ::Union(typs2);
                }
                typ => {
                    let mut typs = ImHashSet::new();
                    typs.insert(t.to_owned());
                    typs.insert(typ);
                    *t = Typ::Union(typs);
                }
            },
        }
    }

    fn solve_constraint(constraint: &Constraint, subst: &mut Subst) -> Vec<Constraint> {
        let mut new_constraints = vec![];

        match constraint {
            Constraint::Eq(t1, t2) => {
                let mut lhs = t1.clone();
                lhs.apply_subst(&subst);
                let mut rhs = t2.clone();
                rhs.apply_subst(&subst);
                Typeinf::unify(subst, &lhs, &rhs);
            }
            Constraint::UnionMem(t1, t2) => {
                let mut lhs = t1.clone();
                lhs.apply_subst(&subst);
                let mut rhs = t2.clone();
                rhs.apply_subst(&subst);

                let n = match rhs {
                    Typ::Metavar(_) => {
                        new_constraints.push(Constraint::UnionMem(lhs, rhs));
                        return new_constraints;
                    }
                    Typ::Unionvar(u) => u,
                    _ => unimplemented!("UnionMem({:?}, {:?})", lhs, rhs),
                };

                Typeinf::subst_insert(subst, n, lhs);
            }
            Constraint::FromMem(t1, field, t2) => {
                let mut lhs = t1.clone();
                lhs.apply_subst(&subst);
                let rhs = t2.clone();

                let typ_vec = match lhs {
                    Typ::Metavar(_) => {
                        new_constraints.push(Constraint::FromMem(lhs, field.clone(), rhs));
                        return new_constraints;
                    }
                    Typ::Unionvar(u) => match subst.get(&u.clone()) {
                        Some(Typ::Object(typ_vec)) => typ_vec.clone(),
                        _ => unimplemented!(),
                    },
                    _ => unimplemented!(),
                };

                // https://www.reddit.com/r/rust/comments/7mqwjn/hashmapstringt_vs_vecstringt/
                let orig = match typ_vec
                    .iter()
                    .find(|x| *x.0.to_string() == field.to_string())
                {
                    Some((_k, v)) => v.clone(),
                    None => panic!("Field not found."),
                };

                new_constraints.append(&mut Typeinf::solve_constraint(
                    &Constraint::Eq(orig.clone(), rhs.clone()),
                    subst
                ));
                //new_constraints.push(Constraint::UnionMem(rhs.clone(), orig.clone()));
            },
            Constraint::IndexMem(t1, t2) => {
                let mut lhs = t1.clone();
                lhs.apply_subst(&subst);
                let rhs = t2.clone();

                let elem_typ = match lhs {
                    Typ::Metavar(_) => {
                        new_constraints.push(Constraint::IndexMem(lhs, rhs));
                        return new_constraints;
                    },
                    Typ::Unionvar(u) => match subst.get(&u.clone()) {
                        Some(Typ::Array(elem_typ)) => *elem_typ.clone(),
                        _ => unimplemented!()
                    },
                    _ => unimplemented!()
                };

                new_constraints.append(&mut Typeinf::solve_constraint(
                    &Constraint::Eq(elem_typ.clone(), rhs.clone()),
                    subst
                ));
            }
        }

        return new_constraints;
    }

    fn solve(constraints: Vec<Constraint>) -> Subst {
        let mut constraints = constraints;
        let mut subst: HashMap<usize, Typ> = HashMap::new();

        loop {
            let mut new_constraints: Vec<Constraint> = vec![];
            for c in constraints.iter() {
                new_constraints.append(&mut Typeinf::solve_constraint(c, &mut subst));
            }
            if new_constraints.len() > 0 {
                constraints = new_constraints;
            } else {
                break;
            }
        }

        let mut at_fixed_point = true;
        loop {
            let mut new_subst: HashMap<usize, Typ> = HashMap::new();
            for (x, t) in subst.iter() {
                let mut t1 = t.clone();
                if t.has_vars() {
                    t1.apply_subst_strict(&subst);
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
    let mut state = Typeinf {
        constraints: vec![],
        next_var: 0,
    };
    let env = ImHashMap::new();
    state.exp(&env, exp)?;
    let subst = Typeinf::solve(state.constraints);
    Typeinf::subst_vars(&subst, exp);
    Ok(())
}

#[cfg(test)]
mod tests {

    use std::collections::HashMap;

    use im_rc::{HashMap as ImHashMap, HashSet as ImHashSet};

    use crate::{
        types::{constructors::*, Exp, Op2, Typ},
        verif::typeinf::typeinf,
    };

    #[test]
    fn union1() {
        let mut e = block(vec![
            let_("x", None, ref_(number(1.0))),
            setref(id("x"), string("hi")),
        ]);
        typeinf(&mut e).unwrap();
        match e.clone() {
            Exp::Block { body } => match &body[..] {
                [Exp::Let {
                    name: _,
                    named: _,
                    typ: Some(t),
                }, _] => {
                    assert!(*t == t_ref(t_union(Typ::F64, Typ::String)));
                }
                _ => panic!("wrong shape"),
            },
            _ => panic!("wrong shape"),
        }
    }

    #[test]
    fn setref_2() {
        let mut hm = HashMap::new();
        hm.insert("x".to_string(), id("x"));

        let mut e = block(vec![
            let_("x", None, ref_(number(10.0))),
            let_("fun", None, ref_(obj(hm))),
            setref(id("x"), bool_(true)),
        ]);

        typeinf(&mut e).unwrap();

        let mut hm2 = HashMap::new();
        hm2.insert("x".to_string(), id("x"));

        let mut hm3 = ImHashMap::new();
        hm3.insert("x".to_string(), t_ref(t_union_2(&[Typ::F64, Typ::Bool])));

        let goal = block(vec![
            let_(
                "x",
                Some(t_ref(t_union_2(&[Typ::F64, Typ::Bool]))),
                ref_(number(10.0)),
            ),
            let_("fun", Some(t_ref(t_obj(hm3))), ref_(obj(hm2))),
            setref(id("x"), bool_(true)),
        ]);

        assert!(e == goal);
    }

    #[test]
    fn setref_3() {
        let mut e = block(vec![
            let_("x", None, ref_(number(10.0))),
            let_("fun", None, ref_(obj_2(&vec![("x", id("x"))]))),
            let_(
                "app",
                None,
                ref_(block(vec![callback(
                    "test",
                    number(0.0),
                    vec![arg("clos", None)],
                    deref(id("fun")),
                    vec![setref(from(deref(id("clos")), "x"), string("tester"))],
                )])),
            ),
        ]);

        typeinf(&mut e).unwrap();

        let goal = block(vec![
            let_(
                "x",
                Some(t_ref(t_union_2(&[Typ::F64, Typ::String]))),
                ref_(number(10.0)),
            ),
            let_(
                "fun",
                Some(t_ref(t_obj_2(&vec![(
                    "x",
                    t_ref(t_union_2(&[Typ::F64, Typ::String])),
                )]))),
                ref_(obj_2(&vec![("x", id("x"))])),
            ),
            let_(
                "app",
                Some(t_ref(Typ::Undefined)),
                ref_(block(vec![callback(
                    "test",
                    number(0.0),
                    vec![arg(
                        "clos",
                        Some(t_ref(t_obj_2(&vec![(
                            "x",
                            t_ref(t_union_2(&[Typ::F64, Typ::String])),
                        )]))),
                    )],
                    deref(id("fun")),
                    vec![setref(from(deref(id("clos")), "x"), string("tester"))],
                )])),
            ),
        ]);

        assert!(e == goal);
    }

    #[test]
    fn setref_4() {
        let mut e = block(vec![
            let_("x", None, ref_(number(10.0))),
            let_("fun", None, ref_(obj_2(&vec![("x", id("x"))]))),
            let_(
                "app",
                None,
                ref_(block(vec![callback(
                    "test",
                    number(0.0),
                    vec![arg("clos", None)],
                    deref(id("fun")),
                    vec![let_(
                        "y",
                        None,
                        ref_(binop(
                            &Op2::Add,
                            deref(from(deref(id("clos")), "x")),
                            string("cheese"),
                        )),
                    )],
                )])),
            ),
        ]);

        typeinf(&mut e).unwrap();

        let goal = block(vec![
            let_("x", Some(t_ref(Typ::F64)), ref_(number(10.0))),
            let_(
                "fun",
                Some(t_ref(t_obj_2(&vec![("x", t_ref(Typ::F64))]))),
                ref_(obj_2(&vec![("x", id("x"))])),
            ),
            let_(
                "app",
                Some(t_ref(Typ::Undefined)),
                ref_(block(vec![callback(
                    "test",
                    number(0.0),
                    vec![arg(
                        "clos",
                        Some(t_ref(t_obj_2(&vec![("x", t_ref(Typ::F64))]))),
                    )],
                    deref(id("fun")),
                    vec![let_(
                        "y",
                        Some(t_ref(t_union_2(&[Typ::F64, Typ::String]))),
                        ref_(binop(
                            &Op2::Add,
                            deref(from(deref(id("clos")), "x")),
                            string("cheese"),
                        )),
                    )],
                )])),
            ),
        ]);

        assert!(e == goal);
    }

    #[test]
    fn while_1() {
        let mut e = block(vec![
            let_("x", None, ref_(number(10.0))),
            let_("z", None, ref_(number(0.0))),
            let_("y", None, ref_(deref(id("z")))),
            while_(
                binop(&Op2::GT, deref(id("x")), number(0.0)),
                vec![
                    setref(id("y"), deref(id("z"))),
                    if_(
                        binop(&Op2::StrictEq, deref(id("x")), number(5.0)),
                        vec![setref(id("z"), bool_(true))],
                        vec![],
                    ),
                    setref(id("x"), binop(&Op2::Sub, deref(id("x")), number(1.0))),
                ],
            ),
        ]);

        typeinf(&mut e).unwrap();

        let goal = block(vec![
            let_("x", Some(t_ref(Typ::F64)), ref_(number(10.0))),
            let_(
                "z",
                Some(t_ref(t_union_2(&[Typ::F64, Typ::Bool]))),
                ref_(number(0.0)),
            ),
            let_(
                "y",
                Some(t_ref(t_union_2(&[Typ::F64, Typ::Bool]))),
                ref_(deref(id("z"))),
            ),
            while_(
                binop(&Op2::GT, deref(id("x")), number(0.0)),
                vec![
                    setref(id("y"), deref(id("z"))),
                    if_(
                        binop(&Op2::StrictEq, deref(id("x")), number(5.0)),
                        vec![setref(id("z"), bool_(true))],
                        vec![],
                    ),
                    setref(id("x"), binop(&Op2::Sub, deref(id("x")), number(1.0))),
                ],
            ),
        ]);

        assert_eq!(e, goal);
    }

    #[test]
    fn objects_1() {
        let mut e = block(vec![
            let_(
                "foo",
                None,
                ref_(obj_2(&[
                    ("x", ref_(number(9.0))),
                    ("y", ref_(number(10.0))),
                ])),
            ),
            let_(
                "bar",
                None,
                ref_(obj_2(&[("x", ref_(string("some test string 123")))])),
            ),
            setref(from(deref(id("foo")), "x"), bool_(true)),
            if_(
                binop(
                    &Op2::StrictEq,
                    deref(from(deref(id("foo")), "x")),
                    bool_(true),
                ),
                vec![setref(
                    from(deref(id("foo")), "y"),
                    deref(from(deref(id("bar")), "x")),
                )],
                vec![unknown()],
            ),
        ]);

        typeinf(&mut e).unwrap();

        let goal = block(vec![
            let_(
                "foo",
                Some(t_ref(t_obj_2(&[
                    ("x", t_ref(t_union_2(&[Typ::F64, Typ::Bool]))),
                    ("y", t_ref(t_union_2(&[Typ::F64, Typ::String]))),
                ]))),
                ref_(obj_2(&[
                    ("x", ref_(number(9.0))),
                    ("y", ref_(number(10.0))),
                ])),
            ),
            let_(
                "bar",
                Some(t_ref(t_obj_2(&[("x", t_ref(Typ::String))]))),
                ref_(obj_2(&[("x", ref_(string("some test string 123")))])),
            ),
            setref(from(deref(id("foo")), "x"), bool_(true)),
            if_(
                binop(
                    &Op2::StrictEq,
                    deref(from(deref(id("foo")), "x")),
                    bool_(true),
                ),
                vec![setref(
                    from(deref(id("foo")), "y"),
                    deref(from(deref(id("bar")), "x")),
                )],
                vec![unknown()],
            ),
        ]);

        assert_eq!(e, goal);
    }

    #[test]
    fn arrays_1() {
        let mut e = block(vec![
            let_(
                "arr",
                None,
                ref_(array(vec![
                    number(9.0),
                    number(10.0)
                ])),
            ),
            set(lval_index(deref(id("arr")), integer(0)), bool_(false)),
        ]);

        typeinf(&mut e).unwrap();

        let goal = block(vec![
            let_(
                "arr",
                Some(t_ref(t_array(t_union_2(&vec![
                    Typ::F64,
                    Typ::Bool
                ])))),
                ref_(array(vec![
                    number(9.0),
                    number(10.0)
                ])),
            ),
            set(lval_index(deref(id("arr")), integer(0)), bool_(false)),
        ]);

        assert_eq!(e, goal);
    }

    #[test]
    fn arrays_2() {
        let mut e = block(vec![let_(
            "arr",
            None,
            ref_(array(vec![
                number(9.0),
                bool_(false)
            ])),
        )]);

        typeinf(&mut e).unwrap();

        let goal = block(vec![
            let_(
                "arr",
                Some(t_ref(t_array(t_union_2(&vec![Typ::F64, Typ::Bool])))),
                ref_(array(vec![
                    number(9.0),
                    bool_(false)
                ])),
            )
        ]);

        assert_eq!(e, goal);
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
