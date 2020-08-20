#!/bin/bash
source ./env
envsubst < fluentbit.yaml.template | microk8s.kubectl apply -f -
echo -e "To follow the logs, run:\n  ./follow_logs.py $SHOW_LOGS_PORT"
