
1. Create program.js in /javascript/examples

```
cd scripts
./prepare_serverless_function.sh ../javascript/examples/echo.js
cd rust/containerless-scaffold
cargo run -- --config '{ "image_name": "serverless-function" }'
curl localhost:8080/hello
```

After 100 curls, it will extract and compile the trace to
`/rust/containerless_scaffold/src/containerless.rs` and then *quit the
program*.

```
cargo run -- --config '{ "image_name": "serverless-function", "initial_state": "Decontainerized" }'
curl localhost:8080/hello
```

If compiling the JS trace fails, see `containerless_scaffold/trace.json` 
If compiling the Rust fails, see `/rust/containerless_scaffold/src/containerless.rs`

# TODO(emily): Add more.
