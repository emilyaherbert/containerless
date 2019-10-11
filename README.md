
1. Create program.js in /javascript/examples

```
cd scripts
./prepare_serverless_function.sh ../javascript/examples/echo.js
cd rust/containerless-scaffold
cargo run -- --config '{ "image_name": "serverless-function" }'
curl localhost:8080/hello
```

After four curls, it will extract and compile the trace to
`/rust/containerless_scaffold/src/containerless.rs` and then *quit the
program*.

```
cargo run -- --config '{ "image_name": "serverless-function", "initial_state": "Decontainerized" }'
curl localhost:8080/hello
```