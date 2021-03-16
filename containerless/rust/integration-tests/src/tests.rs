#![cfg(test)]
use super::test_runner::run_test;
use serde_json::json;

#[test]
fn trivial_fixed_response() {
    let results = run_test(
        "trivialfixedresponse",
        r#"
        let containerless = require("containerless");
        containerless.listen(function(req) {
            containerless.respond("Hello, world!");
        });"#,
        vec![("/hello", json!({}))],
        vec![("/hello", json!({}))],
    );

    assert_eq!(results, vec!["Hello, world!", "Hello, world!"]);
}

#[test]
fn loops() {
    let results = run_test(
        "loops",
        r#"
        let containerless = require("containerless");
        containerless.listen(function(req) {
            let arr = req.body.arr;
            let count = 0;
            for(let i=0; i<arr.length; i++) {
                count = count+1;
            }
            containerless.respond(count);
        });"#,
        vec![("/hello", json!({ "arr": [1, 2, 3] }))],
        vec![("/hello", json!({ "arr": [1, 2, 3] }))],
    );
    assert_eq!(results, vec!["3", "3"]);
}

#[test]
fn yay() {
    let results = run_test(
        "yay",
        r#"
        let containerless = require("containerless");
        containerless.listen(function(req) {
            containerless.respond("yay!");
        });"#,
        vec![("/hello", json!({}))],
        vec![("/hello", json!({}))],
    );
    assert_eq!(results, vec!["yay!", "yay!"]);
}

#[test]
fn frivolous_if() {
    let results = run_test(
        "frivolousif",
        r#"
        let containerless = require("containerless");
        containerless.listen(function(req) {
            let x = 12;
            if(x > 2) {
                x = 11;
            } else {
                x = 13;
            }
            containerless.respond("hi");
        });"#,
        vec![("/hello", json!({}))],
        vec![("/hello", json!({}))],
    );
    assert_eq!(results, vec!["hi", "hi"]);
}

#[test]
#[should_panic]
fn bad_name() {
    let results = run_test(
        "bad_name",
        r#"
        let containerless = require("containerless");
        containerless.listen(function(req) {
            containerless.respond("you won't see this");
        });"#,
        vec![("/hello", json!({}))],
        vec![("/hello", json!({}))],
    );
    assert_eq!(results, vec!["you won't see this", "you won't see this"]);
}

#[test]
fn for_test() {
    let results = run_test(
        "fortest",
        r#"
        let containerless = require("containerless");
        containerless.listen(function(req) {
            let n = req.body.n;
            let x = 0;
            for(let i=0; i<n; i++) {
                x = x + 1;
            }
            containerless.respond("Done!");
        });"#,
        vec![("/hello", json!({"n": 2 }))],
        vec![("/hello", json!({"n": 2 }))],
    );
    assert_eq!(results, vec!["Done!", "Done!"]);
}

#[test]
fn while_test() {
    let results = run_test(
        "whiletest",
        r#"
        let containerless = require("containerless");
        containerless.listen(function(req) {
            let n = req.body.n;
            let x = 0;
            while(n > 0) {
                console.error(n);
                x = x + 1;
                n = n - 1;
            }
            containerless.respond("Done!");
        });"#,
        vec![("/hello", json!({"n": 2 }))],
        vec![("/hello", json!({"n": 2 }))],
    );
    assert_eq!(results, vec!["Done!", "Done!"]);
}

#[test]
fn array_pop() {
    let results = run_test(
        "arraypop",
        r#"
        let containerless = require("containerless");
        containerless.listen(function(req) {
            let x = [0];
            for(let i=0; i<x.length; i++) {
                x.pop();
            }
            containerless.respond("Done!");
        });"#,
        vec![("/hello", json!({}))],
        vec![("/hello", json!({}))],
    );
    assert_eq!(results, vec!["Done!", "Done!"]);
}

#[test]
fn else_before_if() {
    let results = run_test(
        "elsebeforeif",
        r#"
        let containerless = require("containerless");
        containerless.listen(function(req) {
            let x = req.body.x;
            if(x > 10) {
                containerless.respond("42");
                return;
            }
            containerless.respond("24");
        });"#,
        vec![
            ("/hello", json!({ "x": 1 })),
            ("/hello", json!({ "x": 11 })),
        ],
        vec![("/hello", json!({ "x": 11 }))],
    );
    assert_eq!(results, vec!["24", "42", "42"]);
}

#[test]
fn and_bug() {
    let results = run_test(
        "andbug",
        r#"
        let containerless = require("containerless");
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
        vec![("/hello", json!({}))],
        vec![("/hello", json!({}))],
    );
    assert_eq!(results, vec!["good path", "good path"]);
}

#[test]
fn make_adder() {
    let results = run_test(
        "makeadder",
        r#"
        let containerless = require("containerless");
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
        });"#,
        vec![("/hello", json!({}))],
        vec![("/hello", json!({}))],
    );
    assert_eq!(results, vec!["ok", "ok"]);
}

#[test]
fn crazy_make_adder() {
    let results = run_test(
        "crazymakeadder",
        r#"
        let containerless = require("containerless");
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
        });"#,
        vec![("/hello", json!({}))],
        vec![("/hello", json!({}))],
    );
    assert_eq!(results, vec!["ok", "ok"]);
}

#[test]
fn nested_binops() {
    let results = run_test(
        "nestedbinops",
        r#"
        let containerless = require("containerless");
        containerless.listen(function(req) {
            let x = 12;
            if(x > 2 && x < 15) {
                containerless.respond("yay!");
            } else {
                containerless.respond("boo!");
            }
        });"#,
        vec![("/hello", json!({}))],
        vec![("/hello", json!({}))],
    );
    assert_eq!(results, vec!["yay!", "yay!"]);
}

#[test]
fn autocomplete() {
    let results = run_test(
        "autocomplete",
        r#"
        let containerless = require("containerless");
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
                containerless.respond("No matches found!");
            } else {
                containerless.respond(matches);
            }
        });"#,
        vec![("/a", json!({}))],
        vec![("/b", json!({}))],
    );
    assert_eq!(results, vec!["area,art,air", "book,business,body,back,"]);
}

#[test]
fn maze() {
    let results = run_test(
        "maze",
        r#"
        let containerless = require("containerless");
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
            if(maze[y1][x1] > 0 && maze[y2][x2] > 0) {
                let len = findShortestPath(x1, y1, x2, y2);
                if(len === -1) {
                    containerless.respond("No such path exists!\n");
                } else {
                    containerless.respond(len);
                }
            } else {
                containerless.respond("Invalid starting conditions.");
            }
        });"#,
        vec![("/a", json!({"x1": 0, "y1": 0, "x2": 1, "y2": 2}))],
        vec![("/a", json!({"x1": 0, "y1": 0, "x2": 1, "y2": 2}))],
    );
    assert_eq!(results, vec!["3", "3"]);
}

#[test]
fn hello_with_id() {
    let results = run_test(
        "hellowithid",
        r#"
        let containerless = require("containerless");
        containerless.listen(function(req) {
            containerless.helloWithID(req.query.requestID);
        });"#,
        vec![("/?requestID=2", json!({}))],
        vec![("/?requestID=2", json!({}))],
    );

    assert_eq!(results, vec!["Hello, world!", "Hello, world!"]);
}