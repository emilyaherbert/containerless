# Containerless

## Prerequisites

Containerless has components written in Rust and JavaScript (TypeScript).
Thus, you need [Cargo], [Yarn], and [Node]. We package these components
into containers using [Docker].

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
to Docker and MicroK8s, which will involve logging out and logging back in.

*NOTE:* In this demo, the Containerless controller does not run in Kubernetes.
(It is easy to do so, but building a container that runs the Rust
compiler toolchain is a pain.) For the controller to interact with the
rest of Containerless, which runs on Kubernetes, you must run the following
command:

```
microk8s.kubectl config view > ~/.kube/config
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

### Logging

Containerless uses [rsyslog] to consolidate logs from its distributed
components. The `docker/controller.sh` script configures Containerless to to
send log messages to an *rsyslog* server running the local machine, using
UDP port 514 (the default). These log messages get silently discarded
if the *rsyslog* server is not running.

1. Install *rsyslog* with `apt-get install rsyslog`.

2. Open the file `/etc/rsyslog.conf` as root and make two changes:

   a. Add the following lines:
   
      ```
      module(load="imudp")
      input(type="imudp" port="514")
      ```
      
      (By default, they are commented out, so uncomment them.)

   b. At the end of the file, add the lines:

      ```
      $template remote-incoming-logs,"/var/log/rsyslog-%HOSTNAME%.log"
      *.* ?remote-incoming-logs
      & ~
      ```
3. Restart *rsyslog* with `systemctl restart rsyslog`

The logs will now appear in `/var/log/rsyslog-containerless.log`.

## Building

1. Build the containerless system.

   ```
   $ ./build.sh
   ```

## Deploying

The following command deploys Containerless to MicroK8s:

```
./deploy.sh
```

After you run this command, you should see several Pods, Services, and
ReplicaSets running (exact ports and IP addresses will vary):

```
$ microk8s.kubectl get all -n containerless
NAME                         READY   STATUS    RESTARTS   AGE
pod/dispatcher-4lbhm         1/1     Running   0          63s
pod/function-storage-hdqzh   1/1     Running   0          63s

NAME                       TYPE        CLUSTER-IP       EXTERNAL-IP   PORT(S)          AGE
service/dispatcher         NodePort    10.152.183.193   <none>        8080:30373/TCP   63s
service/function-storage   NodePort    10.152.183.118   <none>        8080:32152/TCP   63s
service/kubernetes         ClusterIP   10.152.183.1     <none>        443/TCP          30h

NAME                               DESIRED   CURRENT   READY   AGE
replicaset.apps/dispatcher         1         1         1       63s
replicaset.apps/function-storage   1         1         1       63s
```

The `deploy.sh` script also creates an Ingress, which gives access to 
Containerless Services:

```
$ microk8s.kubectl get ingress
NAME                    HOSTS   ADDRESS     PORTS   AGE
containerless-ingress   *       127.0.0.1   80      2m19s
```

## Integration Tests

To run integration tests, use:

```
$ cd docker && ./test.sh
```

## Invoking Functions (possibly expired)

The Containerless deployment can run the functions in the `/examples`
directory. For example, there is a file called `/examples/hello-world.js`,
thus we can invoke it as follows:

```
$ curl http://localhost/dispatcher/hello-world
Hello world!
```

Note that the first time we invoke the function, it takes 1-2 seconds to *cold
start*. However, subsequent invocations are significantly faster. If we
now run `microk8s.kubectl get all -n containerless` , we will find that the
cluster is now hosting new resources for the `hello-world` function.

## Cleanup

```
cd docker && ./undeploy.sh
```

[Cargo]: https://rustup.rs/
[Yarn]: https://yarnpkg.com/
[Node]: https://nodejs.org/
[Docker]: https://www.docker.com/
[Microk8s]: https://microk8s.io/
[rsyslog]: https://www.rsyslog.com/