# Function Runner Agent

This program is the agent that runs in a container and manages a tracing for
Containerless. The program expects two environment variables to be set:

- `FUNCTION_NAME` the name of the function to run (including the extension).
  The agent fetches this function from
  `http://function-storage/get/FUNCTION_NAME`
- `FUNCTION_MODE`, which should be either `tracing` or `vanilla`.

The agent exposes the following API:

- `http://HOSTNAME:8080/status`: returns code 200 when function is ready
- `http://HOSTNAME:8081/PATH` : invokes the function
- `http://HOSTNAME:8080/trace`: retrieves the execution trace of the serverless
   function.

The agent runs the following external commands within the container:

1. `./js-transform.sh FILENAME`: compiles the serverless function with tracing
   support. Thus, `js-transform.sh` must be a shell script in the working
   directory that calls the compiler.

2. `node FILENAME`: starts the serverless function stored in `FILENAME`. The
   serverless function is assumed to load the Containerless library using
   `require('containerless')`. Therefore, the working directory must have a
   `node_modules/containerless` directory.

## Demo

First, deploy Containerless to microk8s:

```
../../deploy-to-microk8s.sh
```

Start a pod with function-runner:

```
microk8s.kubectl apply -f function-runner-demo.yaml
```

Verify that the function loaded successfully:

```
microk8s.kubectl logs function-runner-demo
```

Setup port forwarding:

```
kubectl port-forward function-runner-demo 8081:8081
```

In a new terminal, invoke the function:

```
curl http://localhost:8081/
```

Cleanup:

```
microk8s.kubectl delete pod function-runner-demo
```
