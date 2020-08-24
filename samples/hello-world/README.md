# Hello World!

This sample application demonstrates how Containerless transparently
transitions from container-based isolation to language-based isolation.
`helloWorld.js` contains a basic example of what a function with Containerless
looks like. `containerless.listen(...)` starts a web server that listens for
incoming requests, and `containerless.hello()` sends a response that indicates
if the function is currently using container-based isolation
(`"Hello from JavaScript~"`) or language-based isolation (`"Hello from Rust!"`).

## Deploying

Deploy the `helloworld` function on Containerless:

```
$ c create -n helloworld -f helloWorld.js
```

The command `create` registers the function to the internal Containerless
storage.

## Invoking

Send a request to the `helloworld` function:

```
$ curl -X POST -H "Content-Type: application/json" "http://localhost/dispatcher/helloworld/hello" -d '{}'
```

This sends a POST request to
`http://localhost/dispatcher/<functionname>/<functionpath>` with an empty body
`'{}'`. In this case, `<functionname>` is `helloworld`, and `<functionpath>` is
`hello`.

## Undeploying

Delete the `helloworld` function from Containerless:

```
$ c delete -n helloworld -f helloWorld.js
```

The `delete` command deletes a function from the internal Containerless storage,
removes its containers, and removes its trace.