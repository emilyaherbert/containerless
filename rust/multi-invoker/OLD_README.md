Decontainerization relies on compiling execution traces from a serverless
function to Rust, and then running that Rust code in the same process as the
invoker. Ideally, we would implement this using dynamic library loading. i.e.,
the invoker process remains active indefinitely, and loads and unloads
libraries of traces as needed. Unfortunately, Rust's support for dynamic
linking appears to be limited. In an an earlier attempt to get dynamic linking
to work, the Rust compiler crashed during static linking. As a workaround, we
follow an alternative approach. We statically link the decontainerized function
to the invoker and dynamically switch between two invoker processes.

There are a couple of ways to accomplish this. The obvious approach would be to
have a proxy process that listens for requests on a public port, and then
copies requests and responses between itself and the invokers. But, the extra
copying is an unreasonable penalty and a threat to validity for any experiment
that measures latency. Therefore, we follow a different approach:

1. We have a monitoring process (this project) that reconfigures the OS
   firewall to directly forward requests from a public port to an invoker
   running on a private port.
2. After the invoker recompiles itself, it sends a signal to the monitor.
3. The monitor starts another invoker process on a new internal port.

Usage example:

```
cargo run -- --config '{ "bind_port": 8080, "config_a": { "bind_port": 8081, "image_name": "serverless-function", "initial_state": "Tracing", "kill_parent": true }, "config_b": { "bind_port": 8082, "image_name": "serverless-function", "initial_state": "Decontainerized" } }'

curl localhost:8080/hi
curl localhost:8080/hi
...
```

