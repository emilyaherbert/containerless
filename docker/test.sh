#!/bin/sh
set -e
set -x
git checkout ../rust/dispatcher-agent/src/decontainerized_functions/mod.rs
(cd ../rust/dispatcher-agent && cargo build)
./restart-dispatcher.sh
./poll-ready.sh http://localhost/dispatcher/readinessProbe 10
(cd ../rust/integration-tests && cargo test)