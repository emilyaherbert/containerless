#![cfg(test)]
use super::benchmark_runner;

#[test]
fn sanity_check() {
    let code = r#"
        let containerless = require("containerless");
        containerless.listen(function(req) {
            containerless.respond("Hello, world!");
        });"#;

    let connections = 10;
    let duration = 60;
    let threads = 4;
    let name = "sanity";

    let wrk_options = benchmark_runner::WrkOptions {
        connections,
        duration,
        threads,
        script_filename: Some("run.lua".to_string()),
        save_wrk_output: true,
    };

    let result = benchmark_runner::run_benchmark(&name, code, "http://google.com", wrk_options);

    assert_eq!(result, Some("Done!".to_string()));
}

#[test]
fn hello_world() {
    let code = r#"
        let containerless = require("containerless");
        containerless.listen(function(req) {
            containerless.hello();
        });"#;

    let connections = 10;
    let duration = 60;
    let threads = 4;
    let name = "helloworld";

    let wrk_options = benchmark_runner::WrkOptions {
        connections,
        duration,
        threads,
        script_filename: Some("run.lua".to_string()),
        save_wrk_output: true,
    };

    let result = benchmark_runner::run_benchmark(
        &name,
        code,
        "http://localhost/dispatcher/helloworld",
        wrk_options,
    );

    assert_eq!(result, Some("Done!".to_string()));
}

#[test]
fn autocomplete() {
    let code = r#"
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
    });"#;

    let connections = 10;
    let duration = 60;
    let threads = 4;
    let name = "autocomplete";

    let wrk_options = benchmark_runner::WrkOptions {
        connections,
        duration,
        threads,
        script_filename: Some("autocomplete.lua".to_string()),
        save_wrk_output: true,
    };

    let result = benchmark_runner::run_benchmark(
        &name,
        code,
        "http://localhost/dispatcher/autocomplete",
        wrk_options,
    );

    assert_eq!(result, Some("Done!".to_string()));
}