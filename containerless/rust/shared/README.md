# Config Options

## Required

- `image_name`

## Optional 

- `bind_port` [8080] - The port at which to host the invoker.
- `initial_state` [Tracing]
    + "DisableTracing" - JS function is executed as normal.
    + "Tracing" - JS function is executed with tracing.
    + "Decontainerized" - The Rust function is ready so lets use that.
- `kill_parent` [false] - Instructs the invoker as to whether it was created
    by a multi-invoker, and thus must kill the auxillary process that spawned
    the invoker in order for the multi-invoker to switch invokers.
- `max_containers` [4] - The maximum number of containers, including the
    possible tracing container.
- `utilization_log` ["utilization.log"] - The output location of the invoker
    CPU and memory utilization log.