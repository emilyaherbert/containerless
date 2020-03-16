Trace-building JS runtime library.

## Setup

To use this library independently of the rest of the system, you need to link
`js-transform` with `yarn`:

```
$ cd ../js-transform
$ yarn run build
$ yarn link
$ cd ../containerless
$ yarn link js-transform
$ yarn run build
```

## Deploying a Single Function

Deploy a single function that builds a trace using Containerless.

1. Apply the `js-transform` transformation.

```
$ ./node_modules/js-transform/dist/index.js examples/echo.js > examples/transformed.js
```

2. Deploy the function and provide a port number.

```
$ node examples/transformed.js 8080
```

3. Send requests to the function:

```
$ curl -X POST -H "Content-Type: application/json" -d '{}' localhost:8080/wassup
/wassup
$ curl -X POST -H "Content-Type: application/json" -d '{}' localhost:8080/foobar
/foobar
```

This can be done any number of times. Requests to the deployed functions must be
`POST` requests, with possibly empty JSON bodies.

4. Extract the trace:

```
$ curl localhost:8080/trace
```

This prints a trace tree for the provided function that complies with the trace
IR [language definition](./ts/exp.ts#L63). This JSON trace tree is recieved by
the [`compiler`](../../rust/compiler) and is deserialized [here](../../rust/compiler/src/test_runner.rs#L111).