#!/bin/sh
cd docker
export EXAMPLES_PATH=$(dirname $(dirname $(realpath $0)))/examples
export RUST_SRC_PATH=$(dirname $(dirname $(realpath $0)))/rust
export CONTROLLER_PORT=7999
export CONTROLLER_IP=$(hostname -I | cut -f 1 -d " ")
echo "Controller is at http://$CONTROLLER_IP:$CONTROLLER_PORT"

envsubst < containerless.yaml | microk8s.kubectl apply -f -
