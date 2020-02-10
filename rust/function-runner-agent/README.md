# Function Runner Agent

This program is the agent that runs in a container and manages a tracing for
Containerless. The agent is a web server with the following high-level API:

1. *init*: Receives the code of a serverless function to run. In the background,
   the agent 1) optionally compiles the function for tracing, 2) and then starts
   the web-server for the function. The *init* function can only be called once,
   and raises an error if called again.

2. *status*: Returns the status of the container, which is one of the following:
   - `"pre-initialized"`: *init* has not been called.
   - `"compiling"`: the function is being compiled (with the trace compiler)
     in the background.
   - `"compile-error"`: trace compilation fails. Any error is reported to
     standard error.
   - `"running"`: the serverless function is ready to receive requests.
   - `"runtime-error"`: the serverless function encountered a runtime error.

   The follow diagram shows how the status may transition:

   ```
   --> pre-initialized ---> compiling ---> running
                              |               |
                              |               v
                              +-------> runtime-error
                              |
                              |
                              +-------> compile-error
   ```

   Thus, all errors are fatal and the container must be shutdown.

3. *get-trace*: Retrieves the execution trace of the serverless
   function. This assumes that *init* successfully compiled the
   serverless function with support for tracing.

The agent runs the following external commands:

1. `./js-transform.sh FILENAME` to compile the serverless function with tracing
   support. Thus, `js-transform.sh` must be a shell script in the working
   directory that calls the compiler.

2. `node FILENAME` where `FILENAME` is a serverless function that uses
   `require('containerless')`. Therefore, the working directory must have a
   `node_modules/containerless` directory.

This directory has a `js-tranform.sh` that runs the program in
`../../javascript/js-transform`. Similarly, `./node_modules/containerless` is a
symlink to `../../javascript/containerless`. So, we can test the agent
in-place.

## Demo

First, start the agent:

```
cargo run
```

Initialize it with a serverless function:

```
curl -X POST -H "Content-Type: application/json" \
    --data $'{ "mode": "Tracing", "code": "let containerless = require(\'containerless\'); containerless.listen(function(req) { console.log(\'Got a response\'); containerless.respond(\'hi\'); });" }' \
    http://localhost:8080/init
```

The command above will create the files `index.js` and `traced.js` in the
current directory.

Check the status of the agent:

```
curl localhost:8080/status
```

Invoke the serverless function:

```
curl localhost:8081/
```

Extract the trace:

```
curl localhost:8080/trace
```

Cleanup:

```
rm index.js traced.js
```

