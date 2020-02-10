# Function Runner

This directory builds a container that holds:

1. The Containerless runtime system,
2. The Containerless JavaScript compiler, and
3. The Containerless Function Runner Agent.

Before building, ensure that all programs in `../rust` and `../javascript`
are already built. To build, run `make`.

To deploy a single function runner as a Pod, use the following manifest:

```
apiVersion: v1
kind: Pod
metadata:
  name: function-runner
spec:
  containers:
  - name: function-runner-name
    image: localhost:5000/function-runner
    ports:
    - containerPort: 8080
    - containerPort: 8081
```