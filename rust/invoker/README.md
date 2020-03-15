The orchestration mechanism for Containerless. `invoker` is used by both
[`containerless-scaffold`](../containerless-scaffold) and
[`multi-invoker`](../multi-invoker) to coordinate between 1) serving requests
from JS, 2) tracing JS code, 3) compiling to Rust, and 4) serving requests from
Rust.

Implementation specific details can be found in the associate Rust doc:

```
cargo doc
```

at your local `target/doc/invoker/index.html`.