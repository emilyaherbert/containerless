#!/bin/sh
export EXAMPLES_PATH=$(dirname $(dirname $(realpath $0)))/examples
export RUST_SRC_PATH=$(dirname $(dirname $(realpath $0)))/rust
envsubst < containerless.yaml | microk8s.kubectl apply -f -