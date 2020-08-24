# Hello World!

This sample application demonstrates how Containerless transparently
transitions from container-based isolation to language-based isolation.
`helloWorld.js` contains a basic example of what a function with Containerless
looks like. `containerless.listen(...)` starts a web server that listens for
incoming requests, and `containerless.hello()` sends a response that indicates
if the function is currently using container-based isolation
(`"Hello from JavaScript!"`) or language-based isolation (`"Hello from Rust!"`).

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

Functions on Containerless can be accessed by requests to
`http://localhost/dispatcher/<functionname>/<functionpath>`. In this case,
`<functionname>` is `helloworld`, and `<functionpath>` is `hello`, and the post
request sends an empty body `{}`.

Now if we check the `containerless` pods running on k8s, we should be able to
see that our initial request has started some instances:

```
$ k get pods -n containerless
NAME                                READY   STATUS    RESTARTS   AGE
controller-logger                   1/1     Running   0          6m23s
dispatcher-77c8b48c4c-9cb27         1/1     Running   0          33s
function-tracing-helloworld         1/1     Running   0          15s
function-vanilla-helloworld-t9hb5   1/1     Running   0          15s
storage-98dnh                       1/1     Running   0          6m22s
```

These pods will shutdown on their own after some amount of time, but for this
demo we will shut them ourselves:

```
$ c remove-containers -n helloworld
```

This will remove all of the running containers for the `helloworld` function.
Note, the `remove-containers` command is for *demo purposes only* and cannot be
use reliably in a production setting. Instead, one should wait for the pods to
go down on their own or use the `delete` command. 

```
$ k get pods -n containerless
NAME                          READY   STATUS    RESTARTS   AGE
controller-logger             1/1     Running   0          8m25s
dispatcher-77c8b48c4c-9cb27   1/1     Running   0          2m35s
storage-98dnh                 1/1     Running   0          8m24s
```

## Undeploying

Delete the `helloworld` function from Containerless:

```
$ c delete -n helloworld -f helloWorld.js
```

The `delete` command deletes a function from the internal Containerless storage,
removes its containers, and removes its trace.