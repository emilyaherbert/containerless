#!/bin/sh
git checkout ../rust/dispatcher-agent/src/decontainerized_functions/mod.rs
(cd ../rust && cargo build)
microk8s.kubectl delete pod --selector "app=controller" -n containerless
echo "You probably want to restart the dispatcher using ./restart-dispatcher.sh"