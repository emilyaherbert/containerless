#!/bin/sh

set -e

cd docker

export RUST_SRC_PATH=$(dirname $(dirname $(realpath $0)))/rust
export CONTROLLER_PORT=7999
export CONTROLLER_IP=$(hostname -I | cut -f 1 -d " ")
export CONTROLLER_LOG_PATH=$(dirname $(realpath $0))

echo "Clearing system state..."
cp "${RUST_SRC_PATH}/dispatcher-agent/src/decontainerized_functions/template.txt" "${RUST_SRC_PATH}/dispatcher-agent/src/decontainerized_functions/mod.rs"
rm "${RUST_SRC_PATH}/dispatcher-agent/src/decontainerized_functions/*.json" 2> /dev/null || true
rm "${RUST_SRC_PATH}/dispatcher-agent/src/decontainerized_functions/*.rs" 2> /dev/null || true

echo "Deploying on k8s..."
envsubst < containerless.yaml | microk8s.kubectl apply -f -

echo "Starting the controller."
echo "This may take some time..."
./controller.sh start
echo "Controller is running at http://localhost/controller\n"

echo "Waiting for dispatcher to come online."
echo "This may take some time..."
./poll-ready.sh http://localhost/dispatcher/readinessProbe 300 || exit 1
echo "Dispatcher is running at http://localhost/dispatcher\n"

echo "System is fully deployed.\n"
