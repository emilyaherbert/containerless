#!/bin/sh

set -e

cd docker

export EXAMPLES_PATH=$(dirname $(dirname $(realpath $0)))/examples
export RUST_SRC_PATH=$(dirname $(dirname $(realpath $0)))/rust
export CONTROLLER_PORT=7999
export CONTROLLER_IP=$(hostname -I | cut -f 1 -d " ")

echo "Clearing system state..."
cp "${RUST_SRC_PATH}/dispatcher-agent/src/decontainerized_functions/template.txt" "${RUST_SRC_PATH}/dispatcher-agent/src/decontainerized_functions/mod.rs"
rm "${RUST_SRC_PATH}/dispatcher-agent/src/decontainerized_functions/*.json" 2> /dev/null || true
rm "${RUST_SRC_PATH}/dispatcher-agent/src/decontainerized_functions/*.rs" 2> /dev/null || true
echo "System state cleared.\n"

echo "Deploying on k8s..."
envsubst < containerless.yaml | microk8s.kubectl apply -f -
echo "Deployed on k8s.\n"

echo "Starting the controller..."
./controller.sh start
echo "Controller is running at http://localhost/controller\n"

echo "Waiting for dispather to come online..."
./poll-ready.sh http://localhost/dispatcher/readinessProbe 300 || exit 1
echo "Dispatcher is running at http://localhost/dispatcher\n"

echo "System is fully deployed, but will take a short time to become operational."
echo "Check system status with 'c status'.\n"
