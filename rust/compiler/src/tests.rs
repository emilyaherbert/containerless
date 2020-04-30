use super::test_runner::TestRunner;
use serde_json::json;
use serial_test_derive::serial;

#[test]
#[serial]
pub fn and_bug() {
    let mut runner = TestRunner::new("and_bug.js");
    let result = runner.test(
        r#"
        let containerless = require("../../javascript/containerless");

        function same(b) {
            return b;
        }

        containerless.listen(function(req) {
            if(same(true) && same(true)) {
                containerless.respond("good path");
            } else {
                containerless.respond("bad path");
            }
        });"#,
        json!([
            { "path": "/hello", "query": {}, "body": {} }
        ]),
        json!([
            { "path": "/hello", "query": {}, "body": {} }
        ]),
    );
    assert_eq!(result, vec!["good path"]);
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
                    let x = "/" + words[i];
                    let match = x.startsWith(req.path);
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

#[serial]
#[test]
pub fn benchmark_maze() {
    let mut runner = TestRunner::new("benchmark_maze.js");
    let result = runner.test(
        r#"
        let containerless = require("../../javascript/containerless");

        let maze =
        [
            [999,   0, 999, 999,   0,   0,   0, 999, 999, 999, 999,   0, 999, 999, 999, 999, 999, 999, 999,   0, 999, 999, 999, 999, 999,   0, 999,   0, 999, 999],
            [999,   0,   0, 999, 999, 999, 999, 999,   0,   0, 999, 999,   0,   0,   0, 999,   0,   0, 999,   0,   0, 999,   0,   0, 999,   0, 999, 999, 999,   0],
            [999, 999,   0, 999,   0,   0,   0,   0,   0,   0,   0, 999, 999, 999,   0, 999,   0,   0, 999,   0,   0, 999,   0, 999, 999,   0, 999,   0, 999,   0],
            [  0, 999,   0, 999, 999, 999,   0, 999, 999,   0, 999, 999,   0, 999,   0, 999, 999,   0, 999,   0, 999, 999,   0, 999,   0,   0, 999,   0, 999, 999],
            [999, 999,   0,   0,   0, 999,   0, 999,   0,   0, 999,   0,   0, 999,   0,   0, 999,   0, 999, 999, 999,   0,   0, 999, 999,   0, 999,   0,   0,   0],
            [999,   0, 999, 999, 999, 999,   0, 999, 999, 999, 999, 999,   0, 999,   0,   0, 999,   0,   0,   0, 999, 999, 999,   0, 999,   0, 999, 999, 999,   0],
            [999,   0, 999,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, 999, 999,   0, 999, 999, 999,   0,   0,   0,   0,   0, 999,   0,   0,   0, 999, 999],
            [999, 999, 999,   0,   0, 999, 999, 999, 999, 999, 999, 999, 999,   0, 999,   0,   0,   0, 999, 999, 999, 999, 999,   0, 999, 999, 999,   0,   0, 999],
            [  0,   0,   0, 999, 999, 999,   0,   0,   0,   0,   0,   0,   0,   0, 999, 999, 999, 999,   0,   0,   0,   0, 999,   0,   0,   0, 999, 999,   0, 999],
            [999, 999,   0, 999,   0, 999,   0, 999,   0, 999, 999, 999, 999, 999,   0,   0,   0, 999,   0, 999, 999, 999, 999,   0, 999,   0,   0, 999,   0, 999],
            [  0, 999,   0, 999,   0, 999, 999, 999,   0, 999,   0, 999,   0,   0,   0,   0, 999, 999,   0,   0,   0, 999,   0,   0, 999, 999,   0, 999,   0, 999],
            [999, 999,   0, 999,   0,   0,   0,   0, 999, 999,   0, 999,   0, 999, 999, 999, 999,   0,   0,   0, 999, 999,   0,   0, 999,   0,   0, 999, 999, 999],
            [999,   0,   0, 999,   0,   0, 999, 999, 999,   0,   0, 999, 999, 999,   0,   0,   0,   0, 999, 999, 999,   0, 999, 999, 999,   0,   0,   0, 999,   0],
            [999, 999,   0, 999,   0, 999, 999,   0,   0,   0,   0,   0,   0,   0,   0,   0, 999, 999, 999,   0,   0,   0,   0,   0, 999,   0, 999, 999, 999,   0],
            [  0, 999,   0, 999,   0, 999,   0, 999, 999, 999, 999, 999, 999, 999,   0, 999, 999,   0, 999, 999,   0, 999, 999, 999, 999,   0, 999,   0,   0, 999],
            [999, 999, 999, 999,   0, 999,   0,   0,   0, 999,   0,   0,   0, 999,   0,   0,   0,   0,   0, 999,   0,   0,   0, 999,   0,   0, 999,   0,   0, 999],
            [999,   0, 999,   0,   0, 999, 999, 999,   0, 999, 999, 999,   0, 999, 999, 999, 999, 999,   0, 999,   0, 999, 999, 999,   0, 999, 999,   0,   0, 999],
            [  0,   0, 999, 999, 999,   0,   0, 999, 999,   0,   0, 999,   0,   0,   0,   0,   0, 999, 999, 999,   0,   0, 999,   0,   0, 999,   0,   0, 999, 999],
            [999, 999, 999,   0, 999,   0,   0,   0, 999, 999,   0, 999, 999,   0, 999, 999,   0,   0,   0,   0, 999, 999, 999, 999, 999, 999, 999, 999, 999,   0],
            [999,   0,   0,   0, 999,   0,   0,   0,   0, 999,   0,   0, 999, 999, 999,   0,   0, 999,   0,   0, 999,   0,   0,   0,   0,   0,   0,   0, 999, 999],
            [999, 999, 999,   0,   0, 999, 999, 999,   0, 999, 999, 999,   0,   0, 999, 999,   0, 999, 999, 999, 999,   0, 999, 999, 999, 999,   0,   0,   0, 999],
            [999,   0, 999,   0,   0, 999,   0, 999,   0,   0,   0, 999,   0, 999,   0, 999, 999,   0,   0,   0,   0, 999, 999,   0,   0, 999,   0, 999, 999, 999],
            [999,   0, 999,   0, 999, 999,   0, 999, 999, 999,   0, 999,   0, 999,   0,   0, 999, 999,   0, 999, 999, 999,   0,   0,   0, 999,   0, 999,   0,   0],
            [999,   0, 999, 999, 999,   0, 999, 999,   0,   0,   0, 999,   0, 999,   0,   0,   0, 999, 999, 999,   0, 999, 999,   0, 999, 999,   0, 999,   0, 999],
            [999,   0,   0,   0,   0,   0, 999,   0, 999, 999, 999, 999,   0, 999,   0,   0,   0,   0,   0,   0,   0,   0, 999,   0, 999,   0,   0, 999, 999, 999],
            [999,   0, 999,   0, 999, 999, 999,   0,   0,   0, 999,   0,   0, 999, 999, 999, 999,   0, 999, 999, 999,   0, 999,   0, 999, 999,   0,   0,   0, 999],
            [999, 999, 999,   0, 999,   0, 999, 999,   0, 999, 999,   0,   0, 999,   0,   0, 999, 999, 999,   0, 999, 999,   0,   0,   0, 999, 999, 999,   0, 999],
            [  0, 999,   0,   0, 999,   0,   0,   0,   0, 999,   0, 999, 999, 999, 999, 999,   0,   0,   0,   0,   0, 999, 999, 999,   0, 999,   0, 999,   0, 999],
            [999, 999, 999,   0, 999, 999,   0, 999, 999, 999,   0, 999,   0,   0,   0,   0,   0, 999, 999, 999,   0,   0,   0, 999,   0,   0,   0, 999,   0, 999],
            [999,   0, 999,   0,   0, 999, 999, 999,   0, 999, 999, 999,   0, 999, 999, 999, 999, 999,   0, 999, 999, 999, 999, 999, 999, 999, 999, 999,   0, 999],
        ];

        function resetMaze() {
            for(let i=0; i<maze.length; i++) {
                for(let j=0; j<maze.length; j++) {
                    if(maze[i][j] > 0) {
                        maze[i][j] = 999;
                    }
                }
            }
        }

        function isValidMove(x, y, turn) {
            if ((x > -1) && (y > -1) && (x < maze[0].length) && (y < maze.length)) {
                return (maze[y][x] > 0) && (turn < maze[y][x]);
            } else {
                return false;
            }
        }

        function findShortestPath(x1, y1, x2, y2) {
            let shortest = -1;

            let moves = [[x1, y1, 0], [x1-1, y1, 0], [x1+1, y1, 0], [x1, y1-1, 0], [x1, y1+1, 0]];

            while(moves.length > 0) {
                let nextMove = moves.shift();
                let x = nextMove[0];
                let y = nextMove[1];
                let turn = nextMove[2] + 1;
                if(isValidMove(x, y, turn)) {
                    if(x === x2 && y === y2) {
                        shortest = turn;
                    } else {
                        maze[y][x] = turn;
                        moves.push([x-1, y, turn]);
                        moves.push([x+1, y, turn]);
                        moves.push([x, y-1, turn]);
                        moves.push([x, y+1, turn]);
                    }
                }
            }

            resetMaze();
            return shortest;
        }

        containerless.listen(function(req) {
            let x1 = req.body.x1;
            let y1 = req.body.y1;
            let x2 = req.body.x2;
            let y2 = req.body.y2;
            // TODO(emily): Fix bug in normalize/ assertNormalize.
            // let and;
            if(maze[y1][x1] > 0 && maze[y2][x2] > 0) {
                let len = findShortestPath(x1, y1, x2, y2);
                if(len === -1) {
                    containerless.respond("No such path exists!\n");
                } else {
                    containerless.respond(len + "\n");
                }
            } else {
                containerless.respond("Invalid starting conditions.\n");
            }
        });"#,
        json!([
            { "path": "/a",
              "query": {},
              "body": {
                    "x1": 0,
                    "y1": 0,
                    "x2": 1,
                    "y2": 2
                }
            },
            { "path": "/a",
              "query": {},
              "body": {
                    "x1": 0,
                    "y1": 0,
                    "x2": 1,
                    "y2": 2
                }
            }
        ]),
        json!([
            { "path": "/a",
              "query": {},
              "body": {
                    "x1": 0,
                    "y1": 0,
                    "x2": 1,
                    "y2": 2
                }
            },
            { "path": "/a",
              "query": {},
              "body": {
                    "x1": 0,
                    "y1": 0,
                    "x2": 1,
                    "y2": 2
                }
            }
        ]));
    assert_eq!(result, ["3", "", "3", ""]);
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
