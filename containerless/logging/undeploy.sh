#!/bin/bash
source ./env
envsubst < fluentbit.yaml.template | microk8s.kubectl delete --ignore-not-found -f -
