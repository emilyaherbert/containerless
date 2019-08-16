
#[cfg(test)]
mod tests {
    use std::fs::File;
    use std::io::prelude::*;
    use std::process::Stdio;
    use std::process::Command;
    use std::fs;
    use std::collections::HashMap;

    use crate::{
        types::{
            Op2,
            Exp,
            constructors::*
        },
        verif::{
            transformer::Transformer,
            assertions::Assertions,
            lift_callbacks::LiftCallbacks
        }
    };

    fn test_harness(filename: &str, code: &str, requests: &str) -> Exp {
        let f = File::create(filename).expect("creating file");
        let mut js_transform = Command::new("node")
            .arg("../tracing/js-transform")
            .stdin(Stdio::piped())
            .stdout(f)
            .spawn()
            .expect("starting js-transform");
        js_transform.stdin.as_mut()
            .expect("opening stdin")
            .write_all(code.as_bytes())
            .expect("failed to write to JS file");
        let exit = js_transform.wait().expect("running js-transform");
        assert!(exit.success());

        let mut decontainerized_js = Command::new("node")
            .arg(filename)
            .arg("test")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .expect("starting decontainerized function");

        decontainerized_js.stdin.as_mut()
            .expect("opening stdin")
            .write_all(requests.as_bytes())
            .expect("failed to write requests");

        let exit = decontainerized_js.wait()
            .expect("running decontainerized function");
        assert!(exit.success(), "non-zero exit code from function");

        let mut stdout = String::new();
        decontainerized_js.stdout.unwrap().read_to_string(&mut stdout)
            .expect("reading stdout");
        // TODO(arjun): We should only remove the file after the calling test
        // case succeeds. How can we do this neatly? Destructors?
        fs::remove_file(filename).expect("removing file");
        let exp = serde_json::from_str::<Exp>(&stdout);

        if exp.is_err() {
            panic!("\n{:?} \nin \n{}", exp, &stdout);
        }


        let mut assertions = Assertions::new();
        let mut transformer = Transformer::new();
        let mut lift_callbacks = LiftCallbacks::new();

        let exp2 = exp.unwrap();

        assertions.assert_supposed_grammar(&exp2);
        assertions.assert_unique_names(&exp2);
        assertions.assert_all_options_are_none(&exp2);

        let mut exp3 = transformer.transform(&exp2);
        //println!("{}", exp3);
        crate::verif::typeinf::typeinf(&mut exp3).unwrap();
        //println!("{}", exp3);


        let exp4 = lift_callbacks.lift(&exp3);

        return exp4;
    }

    #[test]
    pub fn try_test() {
        let _handle = test_harness("try_test.js", r#"
            let containerless = require("../tracing/containerless");
            containerless.listen(function(req, resp) {
                resp('Hello, world!');
            });
        "#, "");
    }

    #[test]
    pub fn try_test2() {
        let handle = test_harness("try_test2.js", r#"
            let containerless = require("../tracing/containerless");
            let str = 'Got a response!';
            containerless.listen(function(req, resp) {
                // console.log(str);
                console.error(str);
                resp(req);
            });
        "#, "request1
        request2");
    }

    #[test]
    pub fn trace_with_unknown() {
        let handle = test_harness("trace_with_unknown.js", r#"
            let containerless = require("../tracing/containerless");
            containerless.listen(function(req, resp) {
                if (req === 'hello') {
                    resp('goodbye');
                }
                else {
                    resp('bad');
                }
            });
        "#, "hello");
        // assert!(false);
    }

    #[test]
        pub fn multiple_callbacks_1() {
        let handle = test_harness("multiple_callbacks_1.js", r#"
            let containerless = require('../tracing/containerless');

            let foo = 'start';
            foo = 42;
            //let foo = 42;

            containerless.listen(function(req, resp) {
                console.error('Got a response');
                let bar = foo + 1;
                resp(req);

            });
        "#, "hello
        goodbye
        hello again");

        }

    #[test]
    pub fn multiple_callbacks() {
        let handle = test_harness("multiple_callbacks.js", r#"
            let containerless = require('../tracing/containerless');

            let foo = 'start';
            foo = 42;
            //let foo = 42;

            containerless.listen(function(req, resp) {
                console.error('Got a response');
                let bar = foo + 1;
                resp(req);

                containerless.get('http://people.cs.umass.edu/~emilyherbert/', function(response) {
                    console.error(response);
                    let baz = foo + bar;
                });

                console.error('All done!');
            });
        "#, "hello
        goodbye
        hello again");

        let mut tenv1 = HashMap::new();
        tenv1.insert("foo00".to_string(), id("foo00"));

        let mut tenv2 = HashMap::new();
        tenv2.insert("bar00".to_string(), id("bar00"));
        tenv2.insert("foo00".to_string(), from(deref(id("$clos")), "foo00"));

        let target = if_(
                binop(&Op2::StrictEq, id("$CBID"), integer(1)),
                vec![
                    let_("$clos", None, index(id("$CBARGS"), integer(0))),
                    let_("$request", None, index(id("$CBARGS"), integer(1))),
                    let_("$responseCallback", None, index(id("$CBARGS"), integer(2))),
                    label("$return", vec![
                        let_("req", None, ref_(deref(id("$request")))),
                        let_("resp", None, ref_(deref(id("$responseCallback")))),
                        prim_app("console.log", vec![from(deref(id("console")), "error"), string("Got a response")]), // console.error is for testing
                        let_("bar00", None, ref_(binop(&Op2::Add, from(deref(id("$clos")), "foo00"), number(1.0)))),
                        let_("app200", None, ref_(block(vec![
                            label("$return", vec![
                                let_("response", None, ref_(deref(id("req")))),
                                prim_app("send", vec![from(deref(id("resp")), "send"), deref(id("response"))])
                            ])
                        ]))),
                        let_("fun100", None, ref_(clos(tenv2))),
                        let_("app300", None, ref_(block(vec![
                            loopback("get", string("http://people.cs.umass.edu/~emilyherbert/"), deref(id("fun100")), 2)
                        ]))),
                        prim_app("console.log", vec![from(deref(id("console")), "error"), string("All done!")]) // console.error is for testing
                    ])
                ],
                vec![
                    if_(
                        binop(&Op2::StrictEq, id("$CBID"), integer(2)),
                        vec![
                            let_("$clos", None, index(id("$CBARGS"), integer(0))),
                            let_("$response", None, index(id("$CBARGS"), integer(1))),
                            label("$return", vec![
                                let_("response", None, ref_(deref(id("$response")))),
                                prim_app("console.log", vec![from(deref(id("console")), "error"), deref(id("response"))]),
                                let_("baz00", None, ref_(binop(&Op2::Add, from(deref(id("$clos")), "foo00"), from(deref(id("$clos")), "bar00"))))
                            ])
                        ],
                        vec![
                            block(vec![
                                let_("foo00", None, ref_(number(42.0))),
                                let_("fun000", None, ref_(clos(tenv1))),
                                let_("app000", None, ref_(block(vec![
                                    loopback("listen", number(0.0), deref(id("fun000")), 1),
                                    unknown()
                                ]))),
                                unknown()
                            ])
                        ]
                    )
                ]
            );

        //println!("{}\n", handle);
        //println!("{}", target);

        assert!(handle == target);

        /*

        {
        "kind": "block",
        "body": [
            {
            "kind": "let",
            "name": "foo00",
            "named": {
                "kind": "number",
                "value": 42
            }
            },
            {
            "kind": "let",
            "name": "fun000",
            "named": {
                "kind": "clos",
                "tenv": {
                "foo00": {
                    "kind": "identifier",
                    "name": "foo00"
                }
                }
            }
            },
            {
            "kind": "let",
            "name": "app000",
            "named": {
                "kind": "block",
                "body": [
                {
                    "kind": "callback",
                    "event": "listen",
                    "eventArg": {
                    "kind": "number",
                    "value": 0
                    },
                    "callbackArgs": [
                    "$clos",
                    "$request",
                    "$responseCallback"
                    ],
                    "clos": {
                    "kind": "identifier",
                    "name": "fun000"
                    },
                    "body": [
                    {
                        "kind": "label",
                        "name": "$return",
                        "body": [
                        {
                            "kind": "let",
                            "name": "req",
                            "named": {
                            "kind": "identifier",
                            "name": "$request"
                            }
                        },
                        {
                            "kind": "let",
                            "name": "resp",
                            "named": {
                            "kind": "identifier",
                            "name": "$responseCallback"
                            }
                        },
                        {
                            "kind": "primApp",
                            "event": "console.log",
                            "eventArgs": [
                            {
                                "kind": "from",
                                "exp": {
                                "kind": "identifier",
                                "name": "console"
                                },
                                "field": "error"
                            },
                            {
                                "kind": "string",
                                "value": "Got a response"
                            }
                            ]
                        },
                        {
                            "kind": "let",
                            "name": "bar00",
                            "named": {
                            "kind": "binop",
                            "op": "+",
                            "e1": {
                                "kind": "from",
                                "exp": {
                                "kind": "identifier",
                                "name": "$clos"
                                },
                                "field": "foo00"
                            },
                            "e2": {
                                "kind": "number",
                                "value": 1
                            }
                            }
                        },
                        {
                            "kind": "let",
                            "name": "app200",
                            "named": {
                            "kind": "block",
                            "body": [
                                {
                                "kind": "label",
                                "name": "$return",
                                "body": [
                                    {
                                    "kind": "let",
                                    "name": "response",
                                    "named": {
                                        "kind": "identifier",
                                        "name": "req"
                                    }
                                    },
                                    {
                                    "kind": "primApp",
                                    "event": "send",
                                    "eventArgs": [
                                        {
                                        "kind": "from",
                                        "exp": {
                                            "kind": "identifier",
                                            "name": "resp"
                                        },
                                        "field": "send"
                                        },
                                        {
                                        "kind": "identifier",
                                        "name": "response"
                                        }
                                    ]
                                    }
                                ]
                                }
                            ]
                            }
                        },
                        {
                            "kind": "let",
                            "name": "fun100",
                            "named": {
                            "kind": "clos",
                            "tenv": {
                                "foo00": {
                                "kind": "from",
                                "exp": {
                                    "kind": "identifier",
                                    "name": "$clos"
                                },
                                "field": "foo00"
                                },
                                "bar00": {
                                "kind": "identifier",
                                "name": "bar00"
                                }
                            }
                            }
                        },
                        {
                            "kind": "let",
                            "name": "app300",
                            "named": {
                            "kind": "block",
                            "body": [
                                {
                                "kind": "callback",
                                "event": "get",
                                "eventArg": {
                                    "kind": "string",
                                    "value": "http:\/\/people.cs.umass.edu\/~emilyherbert\/"
                                },
                                "callbackArgs": [
                                    "$clos",
                                    "$response"
                                ],
                                "clos": {
                                    "kind": "identifier",
                                    "name": "fun100"
                                },
                                "body": [
                                    {
                                    "kind": "label",
                                    "name": "$return",
                                    "body": [
                                        {
                                        "kind": "let",
                                        "name": "response",
                                        "named": {
                                            "kind": "identifier",
                                            "name": "$reponse"
                                        }
                                        },
                                        {
                                        "kind": "primApp",
                                        "event": "console.log",
                                        "eventArgs": [
                                            {
                                            "kind": "from",
                                            "exp": {
                                                "kind": "identifier",
                                                "name": "console"
                                            },
                                            "field": "error"
                                            },
                                            {
                                            "kind": "identifier",
                                            "name": "response"
                                            }
                                        ]
                                        },
                                        {
                                        "kind": "let",
                                        "name": "baz00",
                                        "named": {
                                            "kind": "binop",
                                            "op": "+",
                                            "e1": {
                                            "kind": "from",
                                            "exp": {
                                                "kind": "identifier",
                                                "name": "$clos"
                                            },
                                            "field": "foo00"
                                            },
                                            "e2": {
                                            "kind": "from",
                                            "exp": {
                                                "kind": "identifier",
                                                "name": "$clos"
                                            },
                                            "field": "bar00"
                                            }
                                        }
                                        }
                                    ]
                                    }
                                ]
                                }
                            ]
                            }
                        },
                        {
                            "kind": "primApp",
                            "event": "console.log",
                            "eventArgs": [
                            {
                                "kind": "from",
                                "exp": {
                                "kind": "identifier",
                                "name": "console"
                                },
                                "field": "error"
                            },
                            {
                                "kind": "string",
                                "value": "All done!"
                            }
                            ]
                        }
                        ]
                    }
                    ]
                }
                ]
            }
            }
        ]
        }

        */
    }

}
