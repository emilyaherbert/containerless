# Containerless

## Prerequisites

Containerless has components written in Rust and JavaScript (TypeScript).
Thus, you need to instal[Cargo], [Yarn 1.x], and [Node]. We package these components
into containers using [Docker], which you also need to install. 
We use `jq` in our deployment scripts, which you can install using
`apt-get install jq`.

**Important:** You must build Containerless on Linux. Our build scripts copy 
binaries built on the host into Docker containers, thus they will not run if the
host is running Windows or macOS.

We test Containerless using [MicroK8s], which you can install on Ubuntu
as follows:

```
$ sudo snap install microk8s --classic
$ microk8s.enable dns registry ingress
```

After installation, follow the on-screen directions to gain unprivileged access
to Docker and MicroK8s, which will involve *logging out and logging back in*.

*NOTE:* In this demo, the Containerless controller does not run in Kubernetes.
(It is easy to do so, but building a container that runs the Rust
compiler toolchain is a pain.) For the controller to interact with the
rest of Containerless, which runs on Kubernetes, you must run the following
command:

```
$ microk8s.kubectl config view > ~/.kube/config
```

Then, edit the file to add `insecure-skip-tls-verify: true` below `server: ..`.
For example, the file may look as follows:

```
apiVersion: v1
clusters:
- cluster:
    certificate-authority-data: DATA+OMITTED
    server: https://127.0.0.1:16443
    insecure-skip-tls-verify: true
  name: microk8s-cluster
...
```

In principle, it is possible to deploy Containerless on an arbitrary Kubernetes
cluster. However, our deployment scripts assume you're using MicroK8s.

## Building

Containerless depends on Rust crates that rely on system packages that are not
installed by default on new Linux distributions. If you encounter an error,
while building a dependency, the error message is actually useful. On
Ubuntu 20.04, run the following command before building to avoid errors:

```
sudo apt install libssl-dev pkg-config
```

Build the Containerless system:

```
$ ./build.sh
```

## Logging

Containerless consolidates its logs from its distributed services on a single
pod. To follow the logs, run:

```
microk8s.kubectl -n containerless logs -f controller-logger
```

Two warnings:

1. This command will only work after deployment (described below).

2. A pod may still write output to standard out or standard error,
   which is not captured in the consolidated log.

## Deploying

The following command deploys Containerless to MicroK8s:

```
$ ./deploy.sh
```

After you run this command, you should see several Pods, Services, and
ReplicaSets running (exact ports and IP addresses will vary):

```
$ microk8s.kubectl get all -n containerless
NAME                             READY   STATUS    RESTARTS   AGE
pod/controller-logger            1/1     Running   0          51s
pod/dispatcher-dbcc86d6c-l8j28   1/1     Running   0          21s
pod/storage-fwwf6                1/1     Running   0          51s

NAME                 TYPE        CLUSTER-IP       EXTERNAL-IP   PORT(S)          AGE
service/controller   ClusterIP   10.152.183.196   <none>        80/TCP           51s
service/dispatcher   NodePort    10.152.183.199   <none>        8080:31108/TCP   51s
service/storage      NodePort    10.152.183.144   <none>        8080:31167/TCP   51s

NAME                         READY   UP-TO-DATE   AVAILABLE   AGE
deployment.apps/dispatcher   1/1     1            1           21s

NAME                                   DESIRED   CURRENT   READY   AGE
replicaset.apps/dispatcher-dbcc86d6c   1         1         1       21s
replicaset.apps/storage                1         1         1       51s
```

The `deploy.sh` script also creates an Ingress, which gives access to 
Containerless Services:

```
$ microk8s.kubectl get ingress -n containerless
NAME                    CLASS    HOSTS   ADDRESS     PORTS   AGE
containerless-ingress   <none>   *       127.0.0.1   80      109s
```

## Integration Tests

To run integration tests, use:

```
$ ./test.sh
```

## Containerless CLI

The Containerless CLI bin is located, from the root of the repo, at
`./containerless/rust/target/debug/cli`. We recommend creating a bash alias
`containerless` to this bin. This repo, including documentation and demos,
refers to the CLI bin as the alias `containerless`.

The CLI contains a number of commands:

```
$ containerless --help
Containerless 0.1
Emily Herbert <emilyherbert@cs.umass.edu>, Arjun Guha <a.guha@northeastern.edu>
This doc string acts as a help message when the user runs '--help' as do all doc strings on fields

USAGE:
    cli <SUBCOMMAND>

FLAGS:
    -h, --help       Prints help information
    -V, --version    Prints version information

SUBCOMMANDS:
    compile               Compiles the decontainerized version for a funtion. For testing and demo purposes only
    create                Creates a function
    delete                Delete a function, removes its containers, and removes its compiled trace
    dispatcher-version    Retrieves the current dispatcher version
    get                   Gets the body of a function
    help                  Prints this message or the help of the given subcommand(s)
    invoke                Invokes a function
    list                  Lists all functions
    remove-containers     Removes the containers for a function. For demo purposes only
    remove-trace          Removes the compiled trace for a function. For demo purposes only
```

The most notable are:
- `containerless create -n <name> -f <file>`: Creates a serverless function
  called `<name>` with file `<file>`.
- `containerless delete -n <name>`: Deletes the function `<name>`
- `containerless list`: List the functions currently available

Examples of how to use the CLI and interact with Containerless can be found in
the `samples` directory at the root of the repo.

## Invoking Functions

We can invoke functions by sending requests to the Containerless dispatcher.
In general, functions can be invoked through the url
`http://localhost/dispatcher/<functionname>/<path>?<querykey>=<queryvalue>`.
Given a function `helloworld`, we can invoke it as follows:

```
$ curl -X GET "http://localhost/dispatcher/helloworld"
```

Given a function `fileserver` that POSTs a file to a database and a file
`groceries.json`, we can invoke it as follows:

```
$ curl -X POST -H "Content-Type: application/json" "http://localhost/dispatcher/fileserver/upload?username=<username>&password=<password>&filename=grocerylist" -d @groceries.json
```

Note that the first time we invoke the function, it takes 1-2 seconds to *cold
start*. However, subsequent invocations are significantly faster. If we
now run `microk8s.kubectl get all -n containerless` , we will find that the
cluster is now hosting new resources for the `helloworld` function. More
examples of how to invoke functions can be found in the `samples` directory at
the root of the repo.

## Cleanup

```
$ ./undeploy.sh
```

[Cargo]: https://rustup.rs/
[Yarn 1.x]: https://classic.yarnpkg.com/
[Node]: https://nodejs.org/
[Docker]: https://www.docker.com/
[Microk8s]: https://microk8s.io/
