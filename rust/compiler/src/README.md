1. `cargo run test-tracing -f CODE.js -i INPUT.js > examples/traced.json`
2. `cargo run test-codegen -i examples/traced.json -o ../containerless-scaffold/src/lib.rs`
3. `cd ../containerless-scaffold/ && cargo build && cd ../compiler/`
4. `cargo run test-compiled-trace`