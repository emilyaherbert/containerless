#!/bin/bash

# Follow controller log in the foreground.
tail -f controller.log &
PID=$!

# Follow k8s logs in the background.


# Abort background process on SIGINT (Ctrl + C)
trap 'kill $PID; exit' INT

while true; do
  microk8s.kubectl logs --selector app -n containerless -f
  sleep 1
done

