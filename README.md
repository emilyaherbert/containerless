# Containerless [![](https://github.com/umass-plasma/decontainerization/workflows/CI/badge.svg?branch=emily-wip)]

## Prerequisites

Containerless has components written in Rust and JavaScript (TypeScript).
Thus, you need [Cargo], [Yarn], and [Node]. We package these components
into containers using [Docker].

## Building

```
./build_release.sh
```

## Testing Installation

To test that everything has installed correctly, use:

```
./build_test.sh
```

## Deploying

### Manually Deploying a Function

1. Create a serverless function `program.js` in `/javascript/examples`.

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

## Documentation

## Contributing

[Cargo]: https://rustup.rs/
[Yarn]: https://yarnpkg.com/
[Node]: https://nodejs.org/
[Docker]: https://www.docker.com/