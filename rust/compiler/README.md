## Testing decontainerization by hand

1. Write a serverless function using the containerless library (`main.js`):

```
let containerless = require("../../javascript/containerless");
containerless.listen(function(req, resp) {
    if (req === 'hello') {
        resp('goodbye');
    }
    else {
        resp('bad');
    }
});
```

2. Create a file with one input per line (`input.txt`):

```
hello
goodbye
```

3. Compile the serverless function to generate traces and run it:

```
cargo run test-tracing -f main.js -i input.txt  > trace.json
```

4. Compile the trace to a Rust library:

```
cargo run test-codegen -i trace.json -o ../containerless-scaffold/src/lib.rs 
(cd ../containerless-scaffold && cargo build)
```

5. Run the Rust library on inputs:

```
cargo run test-compiled-trace -f ../containerless-scaffold/target/debug/libcontainerless_scaffold.dylib -i input.txt 
```