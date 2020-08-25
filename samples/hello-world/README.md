# Hello World!

This sample application demonstrates how Containerless transparently
transitions from container-based isolation to language-based isolation.
`helloWorld.js` contains a basic example of what a function with Containerless
looks like. `containerless.listen(...)` starts a web server that listens for
incoming requests, and `containerless.hello()` sends a response that indicates
if the function is currently using container-based isolation
(`"Hello from JavaScript!"`) or language-based isolation (`"Hello from Rust!"`).

Note, the `containerless.hello()` command is for *demo purposes only*, and is
the only component of the Containerless API whose definition changes from
container-based to language-based isolation.

## Deploying

Deploy the `helloworld` function on Containerless:

```
$ containerless create -n helloworld -f helloWorld.js
```

The command `create` registers the function to the internal Containerless
storage. We can ensure that our function has been created correctly by verifying
the body:

```
$ containerless get -n helloworld
let containerless = require('containerless');
containerless.listen(function(req) { 
    containerless.hello();
});
```

## Invoking

Send a request to the `helloworld` function:

```
$ curl -X GET "http://localhost/dispatcher/helloworld"
```

Functions on Containerless can be accessed by requests to
`http://localhost/dispatcher/<functionname>/<functionpath>?<queryparamkey>=<queryparamvalue>`.
In this case, `<functionname>` is `helloworld` and the remaining elements are
empty.

Now if we check the `containerless` pods running on k8s, we should be able to
see that our initial request has started some instances:

```
$ k get pods -n containerless
NAME                                READY   STATUS    RESTARTS   AGE
controller-logger                   1/1     Running   0          3m6s
dispatcher-dbcc86d6c-kc5d7          1/1     Running   0          2m47s
function-tracing-helloworld         1/1     Running   0          10s
function-vanilla-helloworld-ddnhh   1/1     Running   0          10s
storage-b46z9                       1/1     Running   0          3m6s
```

These pods will shutdown on their own after some amount of time, but for this
demo we will shut them ourselves:

```
$ containerless remove-containers -n helloworld
```

This will remove all of the running containers for the `helloworld` function.
Note, the `remove-containers` command is for *demo purposes only* and cannot be
use reliably in a production setting. Instead, you should wait for the pods to
go down on their own or use the `delete` command. We can see that the function
instances have gone down:

```
$ k get pods -n containerless
NAME                         READY   STATUS    RESTARTS   AGE
controller-logger            1/1     Running   0          3m47s
dispatcher-dbcc86d6c-kc5d7   1/1     Running   0          3m28s
storage-b46z9                1/1     Running   0          3m47s
```

## Switching to Language-based Isolation

Now that we can invoke our function, we will want to employ Containerless to
transparently use language-based isolation. Currently, Containerless is
configured to begin tracing the first time that a function is invoked and stop
tracing after 6 requests. At this time, Containerless transparently updates in
the background and switches the `helloworld` function to language-based 
isolation. To demonstrate this, we will invoke our function 6 times:

```
$ curl -X GET "http://localhost/dispatcher/helloworld"
Hello from JavaScript!
$ curl -X GET "http://localhost/dispatcher/helloworld"
Hello from JavaScript!
$ curl -X GET "http://localhost/dispatcher/helloworld"
Hello from JavaScript!
$ curl -X GET "http://localhost/dispatcher/helloworld"
Hello from JavaScript!
$ curl -X GET "http://localhost/dispatcher/helloworld"
Hello from JavaScript!
$ curl -X GET "http://localhost/dispatcher/helloworld"
Hello from JavaScript!
```

Now, Containerless will compile the trace in the background and will
transparently deploy it as Rust code after some amount of time. In the meantime,
we can continue issuing requests, which will be served from container-based
isolation (JavaScript).

After some amount of time, we see:

```
$ curl -X GET "http://localhost/dispatcher/helloworld"
Hello from Rust!
```

We can see that the dispatcher has updated in order to make this change:

```
$ k get pods -n containerless
NAME                                READY   STATUS    RESTARTS   AGE
controller-logger                   1/1     Running   0          6m29s
dispatcher-d8586cff9-hv8lg          1/1     Running   0          22s
function-vanilla-helloworld-9mlbm   1/1     Running   0          71s
storage-b46z9                       1/1     Running   0          6m29s
```

Now, we can remove the compiled trace for `helloworld` to return back to
container-based isolation:

```
$ containerless remove-trace -n helloworld
```

This will remove the compiled trace and will update the dispatcher to make the
change. This is blocking command and is for *demo purposes only*.

If we invoke the function again:

```
$ curl -X GET "http://localhost/dispatcher/helloworld"
Hello from JavaScript!
```

We can see that the dispatcher has updated and new pods for `helloworld` have
been created:

```
$ k get pods -n containerless
NAME                                READY   STATUS    RESTARTS   AGE
controller-logger                   1/1     Running   0          10m
dispatcher-77c8b48c4c-qwg5x         1/1     Running   0          93s
function-tracing-helloworld         1/1     Running   0          52s
function-vanilla-helloworld-6twbl   1/1     Running   0          52s
storage-b46z9                       1/1     Running   0          10m
```

## Undeploying

Delete the `helloworld` function from Containerless:

```
$ containerless delete -n helloworld
```

The `delete` command deletes a function from the internal Containerless storage,
removes its containers, and removes its compiled trace.