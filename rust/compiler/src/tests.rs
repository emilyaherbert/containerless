use serial_test_derive::serial;
use super::test_runner::TestRunner;

#[test]
#[serial]
pub fn trivial_fixed_response() {
    let mut runner = TestRunner::new("trivial_fixed_response.js");
    let result = runner.test(
        r#"
        let containerless = require("../../javascript/containerless");
        containerless.listen(function(req) {
            containerless.respond("Hello, world!");
        });"#,
        "input",
        "input");
    assert_eq!(result, vec!["Hello, world!"]);
}

#[test]
#[serial]
pub fn trivial_echo_path() {
    let mut runner = TestRunner::new("trivial_echo_path.js");
    let result = runner.test(
        r#"
        let containerless = require("../../javascript/containerless");
        containerless.listen(function(req) {
            containerless.respond(req.path);
        });"#,
        "/apath",
        "/bpath");
    assert_eq!(result, ["/bpath"]);
}

#[test]
#[serial]
pub fn trivial_conditional_with_unknown() {
    let mut runner = TestRunner::new("trivial_conditional_with_unknown.js");
    let result = runner.test(
        r#"
        let containerless = require("../../javascript/containerless");
        containerless.listen(function(req) {
            if (req.path === 'hello') {
                containerless.respond("correct");
            }
            else {
                containerless.respond("wrong");
            }
        });
    "#,
        "hello",
        "hello"
    );

    assert_eq!(result, ["correct"]);
}

#[test]
#[serial]
pub fn conditional_return() {
    let mut runner = TestRunner::new("conditional_return.js");
    let result = runner.test(
        r#"
        let containerless = require("../../javascript/containerless");

        function F(x) {
            if (x === 10) {
                return 'ok';
            }
            return 'error';
        }

        containerless.listen(function(req) {
            F(0); // result is ignored
            containerless.respond(F(10));
        });
        "#,
        "ignored", "ignored");
    assert_eq!(result, [ "ok" ]);
}

#[test]
#[serial]
pub fn make_adder() {
    let mut runner = TestRunner::new("make_adder.js");
    let result = runner.test(
        r#"
        let containerless = require("../../javascript/containerless");

        containerless.listen(function(req) {
            function makeAdder(x) {
                return function(y) {
                    return x + y;
                }
            }

            let addTen = makeAdder(10);

            if (addTen(1) === 11) {
                containerless.respond("ok");
            }
            else {
                containerless.respond("error");
            }
        });
        "#,
        "ignored", "ignored");
    assert_eq!(result, [ "ok" ]);
}

#[test]
#[serial]
pub fn nested_binops() {
    let mut runner = TestRunner::new("nested_binops.js");
    let result = runner.test(
        r#"
        let containerless = require("../../javascript/containerless");

        containerless.listen(function(req) {
            let x = 12;
            if(x > 2 && x < 15) {
                containerless.respond("yay!");
            } else {
                containerless.respond("boo!");
            }
        });
    "#,
        "hello",
        "hello"
    );

    assert_eq!(result, ["yay!"]);
}