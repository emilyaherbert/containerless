use super::test_runner::TestRunner;
use serde_json::json;
use serial_test_derive::serial;

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
        json!([
            { "path": "/hello", "query": {}, "body": {} }
        ]),
        json!([
            { "path": "/hello", "query": {}, "body": {} }
        ]),
    );
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
        json!([
            { "path": "/apath", "query": {}, "body": {} }
        ]),
        json!([
            { "path": "/bpath", "query": {}, "body": {} }
        ]),
    );
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
        json!([
            { "path": "hello", "query": {}, "body": {} }
        ]),
        json!([
            { "path": "hello", "query": {}, "body": {} }
        ]),
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
        json!([
            { "path": "/ignored", "query": {}, "body": {} }
        ]),
        json!([
            { "path": "/ignored", "query": {}, "body": {} }
        ]),
    );
    assert_eq!(result, ["ok"]);
}

#[test]
#[serial]
pub fn make_adder_normal() {
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
        json!([
            { "path": "/ignored", "query": {}, "body": {} }
        ]),
        json!([
            { "path": "/ignored", "query": {}, "body": {} }
        ]),
    );
    assert_eq!(result, ["ok"]);
}

#[test]
#[serial]
pub fn crazy_make_adder() {
    let mut runner = TestRunner::new("crazy_make_adder.js");
    let result = runner.test(
        r#"
        let containerless = require("../../javascript/containerless");

        containerless.listen(function(req) {
            function makeAdder(x) {
                return function(y) {
                    x = x + 1; // NOTE: Crazy
                    return x + y;
                }
            }

            let crazy = makeAdder(10);
            let result = crazy(1) + crazy(2); // (11 + 1) + (12 + 2) === 26

            if (result === 26) {
                containerless.respond("ok");
            }
            else {
                containerless.respond("error");
            }
        });
        "#,
        json!([
            { "path": "/ignored", "query": {}, "body": {} }
        ]),
        json!([
            { "path": "/ignored", "query": {}, "body": {} }
        ]),
    );
    assert_eq!(result, ["ok"]);
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
        json!([
            { "path": "/ignored", "query": {}, "body": {} }
        ]),
        json!([
            { "path": "/ignored", "query": {}, "body": {} }
        ]),
    );

    assert_eq!(result, ["yay!"]);
}

#[serial]
#[test]
pub fn login_benchmark() {
    let mut runner = TestRunner::new("login_benchmark.js");
    let result = runner.test(
        r#"
        let containerless = require("../../javascript/containerless");

        containerless.listen(function(req) {
            if(req.path === '/login') {
                containerless.get('data:{ "username": "u", "password": "p" }', function(resp) {
                    if(resp.username === undefined || resp.password === undefined || req.body.username === undefined || req.body.password === undefined) {
                        containerless.respond("Username and password not found.");
                    } else if(resp.username === req.body.username && resp.password === req.body.password) {
                        containerless.respond("Login successful!");
                    } else {
                        containerless.respond("Invalid username or password.");
                    }
                });
            } else {
                containerless.respond("Unknown command.");
            }
        });
        "#,
        json!([
            { "path": "/login",
              "query": {},
              "body": { "username": "u", "password": "p" }  }
        ]),
        json!([
            { "path": "/login",
              "query": {},
              "body": { "username": "u", "password": "p" }  }
        ]));
    assert_eq!(result, ["Login successful!"]);
}

#[serial]
#[test]
pub fn benchmark_status() {
    let mut runner = TestRunner::new("benchmark_status.js");
    let result = runner.test(
        r#"
        let containerless = require("../../javascript/containerless");

        containerless.listen(function(req) {
            let x = 200;
            if(req.path === '/status') {
                if (typeof req.body.username !== 'string') {
                    containerless.respond('Username missing');
                    return;
                }
                if (typeof req.body.token !== 'string') {
                    containerless.respond('Token missing');
                    return;
                }
                if (typeof req.body.state !== 'string') {
                    containerless.respond('State missing');
                    return;
                }

                containerless.get('data:{ "commit": { "sha": "0ce90df7101cb82aa194bad39b614097e30b92ed" } }',
                  function(resp1) {
                    x = x + 1;
                    let y = 200;
                    if (typeof resp1.commit.sha !== 'string') {
                        containerless.respond('SHA missing in resp1');
                        return;
                    }
                    containerless.post({
                        'url': 'data:{}', // 'https://api.github.com/repos/plasma-umass/decontainerization/statuses/' + resp1.commit.sha,
                        'headers': {
                            'User-Agent': req.body.username
                        },
                        'auth': {
                            'username': req.body.username,
                            'password': req.body.token
                        },
                        'body': {
                            'state': req.body.state
                        }
                    }, function(resp2) {
                        let z = x + y;
                        containerless.respond("Done!");
                    });
                });
            } else {
                containerless.respond("Unknown command.");
            }
        });
        "#,
        json!([
            { "path": "/status",
              "query": {},
              "body": { "username": "u", "token": "t", "state": "q" }  }
        ]),
        json!([
            { "path": "/status",
              "query": {},
              "body": { "username": "u", "token": "t", "state": "q" }  }
        ]));
    assert_eq!(result, ["Done!"]);
}
