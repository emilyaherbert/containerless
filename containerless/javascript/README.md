Trace-building JS runtime library.

## Using the `containerless` library independently of the rest of the system

### Setup

To use the `containerless` library independently of the rest of the system, you
need to link `js-transform` with `yarn`, and load the `containerless`
dependency with `npm`.

```
$ cd js-transform
$ yarn install
$ yarn run build
$ yarn link
$ cd ../containerless
$ yarn install
$ yarn link js-transform
$ yarn run build
$ cd ../examples
$ npm install
```

Then create a symbolic link in the `examples` directory:

```
$ ln -s ./node_modules/containerless/node_modules/js-transform/dist/index.js js-transform
```

### Deploying a Function

Deploy a single function that builds a trace using Containerless.

1. Apply the `js-transform` transformation. In the `examples` directory:

```
$ ./js-transform echo.js > transformed.js
```

2. Deploy the function and provide a port number.

```
$ node transformed.js 8080
```

3. Send requests to the function:

```
$ curl -X GET "http://localhost:8080/wassup"
/wassup
$ curl -X GET "http://localhost:8080/foobar"
/foobar
```

This can be done any number of times.

4. Extract the trace:

```
$ curl -X GET "http://localhost:8080/trace"
```

This prints a trace tree for the provided function that complies with the trace
IR [language definition](./containerless/ts/exp.ts#L63). This JSON trace tree is compiled by
the `trace_compiler` inside of the
[`controller-agent`](../rust/controller-agent).