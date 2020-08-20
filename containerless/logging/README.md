# Containerless Logging

To debug and understand how Containerless operates, we need to follow the logs
of all pods simultaneously. Moreover, since Containerless creates and destroys
pods during normal operation, we need to be able to grow and shrink the pods
that we are following dynamically. Unfortunately, this is hard to do in
Kubernetes out of the box.

This directory configures [Fluent Bit](https://fluentbit.io/) to aggregate all
logs and send them over HTTP. We also include a simple HTTP server that displays
the logs on screen.

Run `./deploy.sh` to deploy Fluent Bit to the "containerless-logging" namespace.
You can run `./undeploy.sh` to shut Fluent Bit down. However, it should not
be necessary to do so, unless you are removing Containerless entirely, or
reconfiguring logging in a significant way.

Run `./follow_logs.py` to start the HTTP server that display logs. Note that
you can only run one copy of this server, and that is is configured to listen
on port 8081 by default. You can change this port by editing the `env` file
and redeploying Fluent Bit.