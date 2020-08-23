#!/bin/sh
cd docker

./controller.sh stop

export EXAMPLES_PATH=$(dirname $(dirname $(realpath $0)))/examples
export CONTROLLER_LOG_PATH=$(dirname $(realpath $0))
envsubst < containerless.yaml | microk8s.kubectl delete --ignore-not-found -f -

export RUST_SRC_PATH=$(dirname $(dirname $(realpath $0)))/rust
echo "Clearing system state..."
cp "${RUST_SRC_PATH}/dispatcher-agent/src/decontainerized_functions/template.txt" "${RUST_SRC_PATH}/dispatcher-agent/src/decontainerized_functions/mod.rs"
rm "${RUST_SRC_PATH}/dispatcher-agent/src/decontainerized_functions/*.json" 2> /dev/null || true
rm "${RUST_SRC_PATH}/dispatcher-agent/src/decontainerized_functions/*.rs" 2> /dev/null || true
echo "System state cleared.\n"
