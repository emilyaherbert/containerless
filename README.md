
# Containerless ![](https://github.com/plasma-umass/decontainerization/workflows/CI/badge.svg)

## Overview

This repo is broken into a number of different components:

1. [`js-transform`](./javascript/js-transform/) - Applies ANF transformation and
    instruments JS source code with Containerless runtime statements.
2. [`containerless`](./javascript/containerless/) - Trace-building JS runtime
    library.
3. [`compiler`](./rust/compiler/) - Compiles an execution trace to Rust.
4. [`invoker`](./rust/invoker/) - Orchestrates Containerless serverless
    functions. Either 1) Sends requests to JS for tracing and to be compiled to
    Rust, or 2) Sends requests to Rust. Does not do both simultaneously. This
    also contains the Rust [`trace_runtime`](./rust/invoker/src/trace_runtime)
    library that contains the
    [dynamic type](./rust/invoker/src/trace_runtime/type_dynamic.rs).
5. [`multi-invoker`](./rust/multi-invoker) - Orchestrates Containerless
    serverless functions. Uses two `invoker`'s to send requests to JS while
    functions are being compiled to Rust (#1 above), then send requests to Rust
    (#2 above).
6. [`containerless-scaffold`](./rust/containerless-scaffold/) - The scaffolding
    for the generated Rust code.
7. [`shared`](./rust/shared/) - Components shared between the `invoker` and the
    `multi-invoker`.
8. [`local`](./rust/local/) - Local mock server for running functions and
    experiments.

## Getting Started

### Prerequisites

Containerless has components written in Rust and JavaScript (TypeScript).
Thus, you need [Cargo], [Yarn], and [Node]. We package these components
into containers using [Docker].

### Building

To build all components, use:

```
$ ./build_release.sh
```

## Testing Installation

To test that all components are functioning correctly, use:

```
$ ./build_test.sh
```

## Deploying

### Manually Deploying a Function

1. Create a serverless function `program.js` in [`/javascript/examples`](./javascript/examples/).

2. Build the Containerless [Docker] image.

```
$ ./scripts/prepare_serverless_function.sh javascript/examples/program.js
```

3. Start the server.

```
$ cd rust/containerless-scaffold/
$ cargo run -- --config '{ "image_name": "serverless-function", "initial_state": "Tracing", "max_requests_to_trace": 6, "max_containers": 4 }'
```

This starts an instance of the `invoker` through the proxy of
`containerless-scaffold`. The initial state of the invoker is "Tracing", and it
will trace 6 requests before compiling to Rust. It will use a maximum of 4
containers if necessary, where the tracing container is __not__ included in the
total. A list of config options can be found [here](./rust/shared/README.md).

4. Send requests. All requests to the platform must be sent as `POST` requests,
with possibly empty JSON bodies.

```
$ curl -X POST localhost:8080/hello -d '{}'
```

After 6 curls, it will extract and compile the trace to
`/rust/containerless-scaffold/src/containerless.rs` and then *quit the program*.

5. Serve requests from Rust!

```
$ cargo run -- --config '{ "image_name": "serverless-function", "initial_state": "Decontainerized" }'
$ curl -X POST localhost:8080/hello -d '{}'
```

This starts a different instance of the `invoker` through the proxy of
`containerless-scaffold`. The initial state is "Decontainerized" (i.e. using
language-based isolation via Rust).

### Automatically Deploying a Function

The manual method above allows you to deploy a function to JavaScript, trace and
compile to Rust, and then deploy the function using Rust. However, it involves
manual intervention. In order to automate the process of switching from
JavaScript to Rust, we will employ the `multi-invoker`.

1. Create a serverless function `program.js` in [`/javascript/examples`](./javascript/examples/).

2. Build the Containerless [Docker] image:

```
$ ./scripts/prepare_serverless_function.sh javascript/examples/program.js
```

3. Create the important environment variables:

```
$ CONFIG1="{\"bind_port\":8081,\"image_name\":\"serverless-function\",\"max_containers\":5,\"initial_state\":\"Tracing\",\"kill_parent\":true}"
$ CONFIG2="{\"bind_port\":8082,\"image_name\":\"serverless-function\", \"initial_state\":\"Decontainerized\",\"kill_parent\":true}"
$ CONFIG="{\"bind_port\":8080,\"config_a\":${CONFIG1},\"config_b\":${CONFIG2}}"
```

4. Start the server:

```
$ cd rust/multi-invoker/
$ cargo run -- --config ${CONFIG}
```

This creates a `multi-invoker` (configured with `CONFIG`) that listens for
requests on port 8080. The `multi-invoker` will start two instances of the
`invoker` (the first configured with `CONFIG1` and the second with `CONFIG2`).
The `multi-invoker` will initially direct requests to the first invoker on port
8081 to be traced. When this invoker quits and dies, the `multi-invoker` will
switch and direct requests to the second invoker listening on port 8082. At this
point, the first invoker has already traced and compiled the Rust code, so the
second invoker will be servicing requests directly from the generated Rust
function!

### Debugging

If compiling the JS trace fails, see `containerless_scaffold/trace.json` 
If compiling the Rust fails, see `/rust/containerless_scaffold/src/containerless.rs`

### Using The Local Mock Server

[`local`](./rust/local/) acts as a local mock Datastore and mock Filestore. It
can be started with:

```
$ cargo build
$ cargo run
```

The server is exposed on port 7999, and can be interacted with through various
paths.


[Cargo]: https://rustup.rs/
[Yarn]: https://yarnpkg.com/
[Node]: https://nodejs.org/
[Docker]: https://www.docker.com/
