pub mod transformer;
pub mod typeinf;
//pub mod typeinf2;
//pub mod typeinf3;
pub mod assertions;
pub mod lift_callbacks;

use crate::{
    types::Exp,
    verif::{assertions::Assertions, lift_callbacks::LiftCallbacks, transformer::Transformer},
};

pub fn verify(exp: &Exp) -> Exp {
    let mut assertions = Assertions::new();
    let mut transformer = Transformer::new();
    let mut lift_callbacks = LiftCallbacks::new();

    assertions.assert_supposed_grammar(&exp);
    assertions.assert_unique_names(&exp);
    assertions.assert_all_options_are_none(&exp);

    let mut exp2 = transformer.transform(&exp);
    crate::verif::typeinf::typeinf(&mut exp2).unwrap();
    let exp3 = lift_callbacks.lift(&exp2);

    return exp3;
}

pub fn verify_from_file(filename: &str) -> Exp {
    let json = std::fs::read_to_string(filename)
        .expect(&format!("could not read file {}", filename));
    let exp = serde_json::from_str::<Exp>(&json)
        .unwrap_or_else(|exp| panic!("\n{:?} \nin \n{}", exp, &json));
    return verify(&exp);
}

#[cfg(test)]
mod tests {

    use crate::trace_js::to_exp;
    use crate::{
        types::{constructors::*, Exp, Op2, Typ},
        verif::verify,
    };

    fn test_harness(filename: &str, code: &str, requests: &str) -> Exp {
        let json = to_exp(filename, code, requests);
        // TODO(arjun): We should only remove the file after the calling test
        // case succeeds. How can we do this neatly? Destructors?
        std::fs::remove_file(filename).expect("removing file");
        let exp = serde_json::from_str::<Exp>(&json)
            .unwrap_or_else(|exp| panic!("\n{:?} \nin \n{}", exp, &json));

        let verified = verify(&exp);

        return verified;
    }

    #[test]
    pub fn try_test_1() {
        let handle = test_harness(
            "try_test.js",
            r#"
            let containerless = require("../../javascript/containerless");
            containerless.listen(function(req, resp) {
                resp('Hello, world!');
            });
        "#,
            "start
        stop",
        );

        let goal = if_(
            binop(&Op2::StrictEq, id("arg_cbid"), integer(1)),
            vec![
                let_(
                    "clos",
                    Some(t_ref(t_obj_2(&[]))),
                    ref_(index(id("arg_cbargs"), integer(0))),
                ),
                let_(
                    "request",
                    Some(t_ref(t_obj_2(&[("path", t_ref(Typ::String))]))),
                    ref_(index(id("arg_cbargs"), integer(1))),
                ),
                let_(
                    "responseCallback",
                    Some(t_ref(Typ::ResponseCallback)),
                    ref_(index(id("arg_cbargs"), integer(2))),
                ),
                label(
                    "'return",
                    vec![
                        let_(
                            "req",
                            Some(t_ref(t_obj_2(&[("path", t_ref(Typ::String))]))),
                            ref_(deref(id("request"))),
                        ),
                        let_(
                            "resp",
                            Some(t_ref(Typ::ResponseCallback)),
                            ref_(deref(id("responseCallback"))),
                        ),
                        let_(
                            "app100",
                            Some(t_ref(Typ::Undefined)),
                            ref_(block(vec![label(
                                "'return",
                                vec![
                                    let_(
                                        "response",
                                        Some(t_ref(Typ::String)),
                                        ref_(string("Hello, world!")),
                                    ),
                                    prim_app("send", vec![deref(id("response"))]),
                                ],
                            )])),
                        ),
                    ],
                ),
            ],
            vec![block(vec![
                let_("fun000", Some(t_ref(t_obj_2(&[]))), ref_(obj_2(&[]))),
                let_(
                    "app000",
                    Some(t_ref(Typ::Undefined)),
                    ref_(block(vec![loopback(
                        "listen",
                        number(0.0),
                        deref(id("fun000")),
                        1,
                    )])),
                ),
            ])],
        );

        assert!(handle == goal);
    }

    #[test]
    pub fn try_test_2() {
        let handle = test_harness(
            "try_test_2.js",
            r#"
            let containerless = require("../../javascript/containerless");
            let str = 'Got a response!';
            containerless.listen(function(req, resp) {
                // console.log(str);
                console.error(str);
                resp(req);
            });
        "#,
            "request1
        request2",
        );

        let goal = if_(
            binop(&Op2::StrictEq, id("arg_cbid"), integer(1)),
            vec![
                let_(
                    "clos",
                    Some(t_ref(t_obj_2(&[("str00", t_ref(Typ::String))]))),
                    ref_(index(id("arg_cbargs"), integer(0))),
                ),
                let_(
                    "request",
                    Some(t_ref(t_obj_2(&[("path", t_ref(Typ::String))]))),
                    ref_(index(id("arg_cbargs"), integer(1))),
                ),
                let_(
                    "responseCallback",
                    Some(t_ref(Typ::ResponseCallback)),
                    ref_(index(id("arg_cbargs"), integer(2))),
                ),
                label(
                    "'return",
                    vec![
                        let_(
                            "req",
                            Some(t_ref(t_obj_2(&[("path", t_ref(Typ::String))]))),
                            ref_(deref(id("request"))),
                        ),
                        let_(
                            "resp",
                            Some(t_ref(Typ::ResponseCallback)),
                            ref_(deref(id("responseCallback"))),
                        ),
                        prim_app("console.log", vec![deref(from(deref(id("clos")), "str00"))]),
                        let_(
                            "app200",
                            Some(t_ref(Typ::Undefined)),
                            ref_(block(vec![label(
                                "'return",
                                vec![
                                    let_(
                                        "response",
                                        Some(t_ref(t_obj_2(&[("path", t_ref(Typ::String))]))),
                                        ref_(deref(id("req"))),
                                    ),
                                    prim_app("send", vec![deref(id("response"))]),
                                ],
                            )])),
                        ),
                    ],
                ),
            ],
            vec![block(vec![
                let_(
                    "str00",
                    Some(t_ref(Typ::String)),
                    ref_(string("Got a response!")),
                ),
                let_(
                    "fun000",
                    Some(t_ref(t_obj_2(&[("str00", t_ref(Typ::String))]))),
                    ref_(obj_2(&[("str00", id("str00"))])),
                ),
                let_(
                    "app000",
                    Some(t_ref(Typ::Undefined)),
                    ref_(block(vec![loopback(
                        "listen",
                        number(0.0),
                        deref(id("fun000")),
                        1,
                    )])),
                ),
            ])],
        );

        assert!(handle == goal);
    }

    #[test]
    pub fn trace_with_unknown() {
        let handle = test_harness(
            "trace_with_unknown.js",
            r#"
            let containerless = require("../../javascript/containerless");
            containerless.listen(function(req, resp) {
                if (req === 'hello') {
                    resp('goodbye');
                }
                else {
                    resp('bad');
                }
            });
        "#,
            "hello",
        );

        let goal = if_(
            binop(&Op2::StrictEq, id("arg_cbid"), integer(1)),
            vec![
                let_(
                    "clos",
                    Some(t_ref(t_obj_2(&[]))),
                    ref_(index(id("arg_cbargs"), integer(0))),
                ),
                let_(
                    "request",
                    Some(t_ref(t_obj_2(&[("path", t_ref(Typ::String))]))),
                    ref_(index(id("arg_cbargs"), integer(1))),
                ),
                let_(
                    "responseCallback",
                    Some(t_ref(Typ::ResponseCallback)),
                    ref_(index(id("arg_cbargs"), integer(2))),
                ),
                label(
                    "'return",
                    vec![
                        let_(
                            "req",
                            Some(t_ref(t_obj_2(&[("path", t_ref(Typ::String))]))),
                            ref_(deref(id("request"))),
                        ),
                        let_(
                            "resp",
                            Some(t_ref(Typ::ResponseCallback)),
                            ref_(deref(id("responseCallback"))),
                        ),
                        if_(
                            binop(&Op2::StrictEq, deref(id("req")), string("hello")),
                            vec![let_(
                                "app100",
                                Some(t_ref(Typ::Undefined)),
                                ref_(block(vec![label(
                                    "'return",
                                    vec![
                                        let_(
                                            "response",
                                            Some(t_ref(Typ::String)),
                                            ref_(string("goodbye")),
                                        ),
                                        prim_app("send", vec![deref(id("response"))]),
                                    ],
                                )])),
                            )],
                            vec![unknown()],
                        ),
                    ],
                ),
            ],
            vec![block(vec![
                let_("fun000", Some(t_ref(t_obj_2(&[]))), ref_(obj_2(&[]))),
                let_(
                    "app000",
                    Some(t_ref(Typ::Undefined)),
                    ref_(block(vec![loopback(
                        "listen",
                        number(0.0),
                        deref(id("fun000")),
                        1,
                    )])),
                ),
            ])],
        );

        assert!(handle == goal);
    }

    #[test]
    pub fn multiple_callbacks_1() {
        let handle = test_harness(
            "multiple_callbacks_1.js",
            r#"
            let containerless = require('../../javascript/containerless');

            let foo = 'start';

            containerless.listen(function(req, resp) {
                console.error('Got a response');
                foo = 42;
                let bar = foo + 1;
                resp(req);

            });

            foo = false;
        "#,
            "hello
        goodbye
        hello again",
        );

        let goal = if_(
            binop(&Op2::StrictEq, id("arg_cbid"), integer(1)),
            vec![
                let_(
                    "clos",
                    Some(t_ref(t_obj_2(&[(
                        "foo00",
                        t_ref(t_union_2(&[Typ::String, Typ::F64, Typ::Bool])),
                    )]))),
                    ref_(index(id("arg_cbargs"), integer(0))),
                ),
                let_(
                    "request",
                    Some(t_ref(t_obj_2(&[("path", t_ref(Typ::String))]))),
                    ref_(index(id("arg_cbargs"), integer(1))),
                ),
                let_(
                    "responseCallback",
                    Some(t_ref(Typ::ResponseCallback)),
                    ref_(index(id("arg_cbargs"), integer(2))),
                ),
                label(
                    "'return",
                    vec![
                        let_(
                            "req",
                            Some(t_ref(t_obj_2(&[("path", t_ref(Typ::String))]))),
                            ref_(deref(id("request"))),
                        ),
                        let_(
                            "resp",
                            Some(t_ref(Typ::ResponseCallback)),
                            ref_(deref(id("responseCallback"))),
                        ),
                        prim_app("console.log", vec![string("Got a response")]),
                        setref(from(deref(id("clos")), "foo00"), number(42.0)),
                        let_(
                            "bar00",
                            Some(t_ref(t_union_2(&[Typ::String, Typ::F64, Typ::Bool]))),
                            ref_(binop(
                                &Op2::Add,
                                deref(from(deref(id("clos")), "foo00")),
                                number(1.0),
                            )),
                        ),
                        let_(
                            "app200",
                            Some(t_ref(Typ::Undefined)),
                            ref_(block(vec![label(
                                "'return",
                                vec![
                                    let_(
                                        "response",
                                        Some(t_ref(t_obj_2(&[("path", t_ref(Typ::String))]))),
                                        ref_(deref(id("req"))),
                                    ),
                                    prim_app("send", vec![deref(id("response"))]),
                                ],
                            )])),
                        ),
                    ],
                ),
            ],
            vec![block(vec![
                let_(
                    "foo00",
                    Some(t_ref(t_union_2(&[Typ::String, Typ::F64, Typ::Bool]))),
                    ref_(string("start")),
                ),
                let_(
                    "fun000",
                    Some(t_ref(t_obj_2(&[(
                        "foo00",
                        t_ref(t_union_2(&[Typ::String, Typ::F64, Typ::Bool])),
                    )]))),
                    ref_(obj_2(&[("foo00", id("foo00"))])),
                ),
                let_(
                    "app000",
                    Some(t_ref(Typ::Undefined)),
                    ref_(block(vec![loopback(
                        "listen",
                        number(0.0),
                        deref(id("fun000")),
                        1,
                    )])),
                ),
                setref(id("foo00"), bool_(false)),
            ])],
        );

        assert_eq!(handle, goal);
    }
}
