# Containerless ![](https://github.com/plasma-umass/decontainerization/workflows/CI/badge.svg)

## Overview

This repo is broken into a number of different components:

1. [`js-transform`](./javascript/js-transform/) - Applies ANF transformation and
    instruments JS source code with Containerless runtime statements.
2. [`containerless`](./javascript/containerless/) - Trace-building JS runtime
    library.
3. [`compiler`](./rust/compiler/) - Compiles an execution trace to Rust.
4. [`invoker`](./rust/invoker/) - Orchestrates Containerless serverless
    functions. Sends requests to JS while functions are being compiled to Rust,
    then sends requests to Rust.
5. [`multi-invoker`](./rust/multi-invoker) - Something.
6. [`containerless-scaffold`](./rust/containerless-scaffold/) - Something.
7. [`local`](./rust/local/) - Something.
8. [`shared`](./rust/shared/) - Something.

## Getting Started

### Prerequisites

Containerless has components written in Rust and JavaScript (TypeScript).
Thus, you need [Cargo], [Yarn], and [Node]. We package these components
into containers using [Docker].

### Building

To build all components, use:

```
./build_release.sh
```

To build individual components, use one of the following commands in a
respective directory:

```
yarn run build
cargo build
```

## Testing Installation

To test that all components are functioning correctly, use:

```
./build_test.sh
```

To test individual components, use one of the following commands in a respective directory:

```
yarn run test
cargo test
```

## Deploying

### Manually Deploying a Function

1. Create a serverless function `program.js` in [`/javascript/examples`](./javascript/examples/).

2. Build the Containerless [Docker] image.

```
./scripts/prepare_serverless_function.sh javascript/examples/program.js
```

3. Start the server.

```
cd rust/containerless-scaffold
cargo run -- --config '{ "image_name": "serverless-function", "max_requests_to_trace": 6 }'
```

4. Send requests. All requests to the platform must be sent as `POST` requests,
with possibly empty JSON bodies.

```
curl -X POST localhost:8080/hello -d '{}'
```

After 6 curls, it will extract and compile the trace to
`/rust/containerless_scaffold/src/containerless.rs` and then *quit the
program*.

5. Serve requests from Rust!

```
cargo run -- --config '{ "image_name": "serverless-function", "initial_state": "Decontainerized" }'
curl -X POST localhost:8080/hello -d '{}'
```

### Debugging

If compiling the JS trace fails, see `containerless_scaffold/trace.json` 
If compiling the Rust fails, see `/rust/containerless_scaffold/src/containerless.rs`

## Contributing

[Cargo]: https://rustup.rs/
[Yarn]: https://yarnpkg.com/
[Node]: https://nodejs.org/
[Docker]: https://www.docker.com/