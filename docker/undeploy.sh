#!/bin/sh
export EXAMPLES_PATH=$(dirname $(dirname $(realpath $0)))/examples
envsubst < containerless.yaml | microk8s.kubectl delete -f -