A platform for running serverless functions. Usage:

```
cargo run IMAGE-NAME
curl localhost:8080
```

IMAGE-NAME should be the name of a Docker image that creates an HTTP
server at port 3000. (There a CLI argument to change the expected port number.)

Run `cargo run -- --help` for many options. The most significant is
the `--max-containers` flag, which controls the level of parallelism.

# TODO(emily): Add more.
