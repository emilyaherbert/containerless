Containerless Design
====================

Containerless has several features:

1. It can run serverless functions in isolated containers, and
   transparently scale them up or down, including scale to zero.
   
2. It can instrument serverless functions to build execution
   traces, extract traces, and compile them to Rust.

3. It can transparently switch from running serverless functions
   in containers to serverless functions in Rust.

4. It can run on a multi-node cluster.

The trace instrumentation and compilation (Requirement 2) is the most
significant programming languages contribution and is detailed in our paper.
This document describes the rest of the system design.

Containerless is built on Kubernetes, which makes it easy to (1) run on a
multi-node cluster, (2) run serverless functions in containers, and (3)
transparently scale up and down (but not to zero). Therefore, we spend
significant effort to support (1) scaling down to zero, and (2) transparently
switching from containers to Rust.

To scale to zero, Containerless has to (a) cold-start functions when needed and
(b) destroy idle functions. The approach we take is relatively straightforward.
If a function F is cold and new requests arrive for F, we enqueue them until
the function is available. When F is warm, we use a sliding widnow to track the
number of concurrent requests to F, which determines how many replicas to create.
When the number of desired replicas reaches zero, we destroy all resources
(including k8s resources) dedicated to F.

Unfortunately, transparently switching from containers to Rust is quite
complicated, because Rust does not support dynamic linking. Instead, we have to
statically link decontainerized functions to Rust, which involves restarting
the *dispatcher* process that receives serverless function requests. However,
we exploit Kubernetes to make this process simpler and more reliable. We use a
Kubernetes *deployment* to manage the dispatcher. Therefore, when a new
statically-linked dispatcher is ready, we update the deployment, and Kubernetes
takes care of directing network traffic from the old dispatcher to the new
dispatcher, when it is ready.

The rest of this document describes this design in more detail.

---

Containerless consists of four container images:

1. The *controller* includes the Rust compiler toolchain, the trace compiler, and 
   the source code of the Dispatcher. On startup, it builds the `dispatcher-agent`
   executable and then starts a web server that:
   
   - Serves the `dispatcher-agent` executable, and
   - Receives a trace from the running Dispatcher, compiles the trace to Rust,
     rebuilds the Dispatcher executable, and then updates the Dispatcher 
     deployment, which prompts the new Dispatcher to download the latest
     executable.

1. The *dispatcher* has an executable, `dispatcher-launcher-agent`, which
   immediately downloads and runs the latest `dispatcher` from the controller.
   On startup, the `dispatcher-agent` queries Kubernetes for existing
   functions, which it then "adopts". On shutdown, the `dispatcher-agent`
   does not destroy the Kubernetes resources that it creates, so that the
   next version of the `dispatcher-agent` can adopt them. While running,
   The `dispatcher-agent` receives requests for serverless functions. It
   processes them locally -- as decontainerized functions -- when possible, and
   forwards requests to other pods if needed. The dispatcher takes care of
   creating Function Runners to process requests, marks certain Function Runners
   as tracing pods, extracts traces, and sends  them to the Controller service.

3. The *storage* image runs a small web server that hosts the JavaScript
   code for function

4. The *function-runner* runs a single serverless function and includes the trace
   compiler toolchain (`javasript/js-transform`). It receives two arguments
   (via environment variables) that specify the (a) the name of the function,
   and (b) whether or not to enable tracing. During startup, the Function
   Runner fetches the JavaScript code for the function from the Storage service,
   optionally compiles it using the trace compiler, and then starts the
   function.