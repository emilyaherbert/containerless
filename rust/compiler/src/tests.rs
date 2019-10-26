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
                        containerless.respond('Uploaded');
                    });
                } else {
                    containerless.respond("No file to upload.");
                }
            } else {
                containerless.respond("Unknown command.");
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
    assert_eq!(result, ["Uploaded"]);
}

#[serial]
#[test]
pub fn benchmark_autocomplete() {
    let mut runner = TestRunner::new("benchmark_autocomplete.js");
    let result = runner.test(
        r#"
        let containerless = require("../../javascript/containerless");

        let words = [
            "time",
            "year",
            "people",
            "way",
            "day",
            "man",
            "thing",
            "woman",
            "life",
            "child",
            "world",
            "school",
            "state",
            "family",
            "student",
            "group",
            "country",
            "problem",
            "hand",
            "part",
            "place",
            "case",
            "week",
            "company",
            "system",
            "program",
            "question",
            "work",
            "government",
            "number",
            "night",
            "point",
            "home",
            "water",
            "room",
            "mother",
            "area",
            "money",
            "story",
            "fact",
            "month",
            "lot",
            "right",
            "study",
            "book",
            "eye",
            "job",
            "word",
            "business",
            "issue",
            "side",
            "kind",
            "head",
            "house",
            "service",
            "friend",
            "father",
            "power",
            "hour",
            "game",
            "line",
            "end",
            "member",
            "law",
            "car",
            "city",
            "community",
            "name",
            "president",
            "team",
            "minute",
            "idea",
            "kid",
            "body",
            "information",
            "back",
            "parent",
            "face",
            "others",
            "level",
            "office",
            "door",
            "health",
            "person",
            "art",
            "war",
            "history",
            "party",
            "result",
            "change",
            "morning",
            "reason",
            "research",
            "girl",
            "guy",
            "moment",
            "air",
            "teacher",
            "force",
            "education",
        ];

        containerless.listen(function(req) {
            let matches = [];
            for(let i=0; i<words.length; i = i + 1) {
                let word = words[i];
                if((req.path.length > 1) && word.length >= (req.path.length - 1)) {
                    let j = 0;
                    let match = true;
                    while(j < (req.path.length - 1)) {
                        if(word[j] !== req.path[j+1]) {
                            match = false;
                            j = req.path.length;
                        } else {
                            j = j + 1;
                        }
                    }
                    if(match) {
                        matches.push(word);
                    }
                }
            }
            if(matches.length === 0) {
                containerless.respond("No matches found!\n");
            } else {
                containerless.respond(matches + "\n");
            }
        });"#,
        json!([
            { "path": "/a",
              "query": {},
              "body": ""  }
        ]),
        json!([
            { "path": "/a",
              "query": {},
              "body": ""  }
        ]));
    assert_eq!(result, ["area,art,air,", ""]);
}
