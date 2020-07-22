use super::test_runner::TestRunner;
use serde_json::json;
use serial_test_derive::serial;


#[serial]
#[test]
pub fn benchmark_login() {
    let mut runner = TestRunner::new("benchmark_login.js");
    let result = runner.test(
        r#"
        let containerless = require("../../javascript/containerless");

        containerless.listen(function(req) {
            if(req.path === '/login') {
                let u = req.body.username;
                let p = req.body.password;
                if(u === undefined || p === undefined) {
                    containerless.respond("Username or password not found.\n")
                } else {
                    containerless.post({
                            url: 'data:{ "username": "timmy", "password": "rust" }',
                            body: req.body
                        }, function(resp) {
                        containerless.respond(u === resp.username);
                        if(resp.username === undefined || resp.password === undefined) {
                            containerless.respond(resp.body);
                        } else if(resp.username === u && resp.password === p) {
                            containerless.respond("Login successful!");
                        } else {
                            containerless.respond(resp.body);
                        }
                    });
                }
            } else {
                containerless.respond("Unknown command.");
            }
        });"#,
        json!([
            { "path": "/login",
              "query": {},
              "body": { "username": "timmy", "password": "rust" }  }
        ]),
        json!([
            { "path": "/login",
              "query": {},
              "body": { "username": "timmy", "password": "rust" }  }
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
              "body": { "username": "u", "token": "t", "state": "q" }  },
            { "path": "/status",
              "query": {},
              "body": { "username": "u", "token": "t" }  }
        ]),
        json!([
            { "path": "/status",
              "query": {},
              "body": { "username": "u", "token": "t", "state": "q" }  },
            { "path": "/status",
              "query": {},
              "body": { "username": "u", "token": "t" }  }
        ]));
    assert_eq!(result, ["Done!", "State missing"]);
}

#[serial]
#[test]
pub fn benchmark_upload() {
    let mut runner = TestRunner::new("benchmark_upload.js");
    let result = runner.test(
        r#"
        let containerless = require("../../javascript/containerless");

        containerless.listen(function(req) {
            if(req.path === '/upload') {
                if(req.body !== undefined) {
                    containerless.post({
                        url: 'data:{}',
                        body: req.body
                    }, function(resp) {
                        containerless.respond("Uploaded!");
                    });
                } else {
                    containerless.respond("No file to upload.\n");
                }
            } else {
                containerless.respond("Unknown command.\n");
            }
        });"#,
        json!([
            { "path": "/upload",
              "query": {},
              "body": "This a a plain text file"  }
        ]),
        json!([
            { "path": "/upload",
              "query": {},
              "body": "This is another file"  }
        ]));
    assert_eq!(result, ["Uploaded!"]);
}

#[serial]
#[test]
pub fn benchmark_banking() {
    let mut runner = TestRunner::new("benchmark_banking.js");
    let result = runner.test(
        r#"
        let containerless = require("../../javascript/containerless");

        /**
         * Begins a request.
         *
         * `next(req, transaction)`
         */
        function begin(req, next) {
            containerless.get('data:{ "transaction": "9876543210" }', function(resp) {
                if(resp === undefined) {
                    console.log(resp);
                    containerless.respond("No response. 1");
                    return;
                }
                if(resp.transaction === undefined) {
                    containerless.respond("No transaction 1.");
                    return;
                }
                next(req, resp.transaction);
            });
        }

        /**
         * Commits a request.
         */
        function commit(req, transaction, mutation) {
            let o = {
                'url': 'data:{ "body": "Done!" }',
                'body': {
                    "transaction": transaction,
                    "mutation": mutation
                }
            };
            containerless.post(o, function(resp) {
                if(resp === undefined) {
                    containerless.respond("No response 2.");
                    return;
                }
                if(resp.body !== undefined) {
                    containerless.respond(resp.body);
                    return;
                }
                containerless.respond("Something happened but not sure what.");
            });
        }

        /**
         * Finds the current balance.
         *
         * `next(resp);`
         */
        function balance(req, transaction, next) {
            let o = {
                'url': 'data:{ "balance": 81900 }',
                'body': {
                    'name': req.body.account
                }
            };
            containerless.post(o, function(resp) {
                if(resp === undefined) {
                    containerless.respond("No response 3.");
                    return;
                }
                next(resp);
            });
        }

        /**
         * Performs a withdrawl.
         */
        function withdraw(req, transaction) {
            balance(req, transaction, function(resp) {
                if(resp === undefined) {
                    containerless.respond("No response 4.");
                    return;
                }
                if(resp.balance === undefined) {
                    containerless.respond("No balance 4.");
                    return;
                }
                let newBalance = resp.balance - (100 * req.query.amount);
                let o = {
                    'name': req.body.account,
                    'balance': newBalance
                };
                commit(req, transaction, o);
            });
        }

        /**
         * Performs a deposit.
         */
        function deposit(req, transaction) {
            balance(req, transaction, function(resp) {
                if(resp === undefined) {
                    containerless.respond("No response 5.");
                    return;
                }
                if(resp.balance === undefined) {
                    containerless.respond("No balance 5.");
                    return;
                }
                let newBalance = resp.balance - (-100 * req.query.amount);
                let o = {
                    'name': req.body.account,
                    'balance': newBalance
                };
                commit(req, transaction, o);
            });
        }

        containerless.listen(function(req) {
            if(req.body === undefined) {
                containerless.respond("Undefined body.");
                return;
            }
            if(req.body.account === undefined) {
                containerless.respond("Account undefined.");
                return;
            }
            if(req.path === '/balance') {
                begin(req, function(req, transaction) {
                    balance(req, transaction, function(resp) {
                        if(resp.balance === undefined) {
                            containerless.respond("No balance 6.");
                            return;
                        }
                        containerless.respond(resp.balance / 100.0);
                    });
                });
            } else if(req.path === '/withdraw') {
                if(req.query === undefined) {
                    containerless.respond("Query undefined 1.");
                    return;
                }
                if(req.query.amount === undefined) {
                    containerless.respond("Amount undefined 1.");
                    return;
                }
                begin(req, withdraw);
            } else if(req.path === '/deposit') {
                if(req.query === undefined) {
                    containerless.respond("Query undefined 2.");
                    return;
                }
                if(req.query.amount === undefined) {
                    containerless.respond("Amount undefined 2.");
                    return;
                }
                begin(req, deposit);
            } else {
                containerless.respond("Unknown command.\n");
            }
        });"#,
        json!([
            { "path": "/balance",
              "query": {},
              "body": {
                    "accessToken": "letmein",
                    "account": "timmy"
                }
            }
        ]),
        json!([
            { "path": "/balance",
              "query": {},
              "body": {
                    "accessToken": "letmein",
                    "account": "timmy"
                }
            }
        ]));
    assert_eq!(result, ["819"]);
}
