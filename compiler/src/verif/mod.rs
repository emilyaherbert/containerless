pub mod transformer;
pub mod typeinf;
pub mod assertions;
pub mod lift_callbacks;

use crate::{
    types::Exp,
    verif::{
        transformer::Transformer,
        assertions::Assertions,
        lift_callbacks::LiftCallbacks
    }
};

pub fn verify(exp: &Exp) -> Exp {
    let mut assertions = Assertions::new();
    let mut transformer = Transformer::new();
    let mut lift_callbacks = LiftCallbacks::new();

    assertions.assert_supposed_grammar(&exp);
    assertions.assert_unique_names(&exp);
    assertions.assert_all_options_are_none(&exp);

    let mut exp2 = transformer.transform(&exp);
    //println!("{}", exp3);
    crate::verif::typeinf::typeinf(&mut exp2).unwrap();
    //println!("{}", exp3);


    let exp3 = lift_callbacks.lift(&exp2);

    return exp3;
}

#[cfg(test)]
mod tests {

    use crate::{
        types::{
            Exp,
            to_exp
        },
        verif::verify
    };

    fn test_harness(filename: &str, code: &str, requests: &str) -> Exp {
        let exp = to_exp(filename, code, requests);
        let verified = verify(&exp);

        return verified;
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
}