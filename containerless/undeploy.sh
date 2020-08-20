#!/bin/sh
cd docker

./controller.sh stop

export EXAMPLES_PATH=$(dirname $(dirname $(realpath $0)))/examples
export CONTROLLER_LOG_PATH=$(dirname $(realpath $0))
envsubst < containerless.yaml | microk8s.kubectl delete --ignore-not-found -f -
