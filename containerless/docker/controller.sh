#!/bin/bash
export RUST_LOG=info
case $1 in
start)
    if [ -f .controller.pid ]; then
        echo "Controller already running"
        exit 1
    fi
    cd ../rust
    ./target/debug/controller-agent &> ../docker/controller.log &
    cd ../docker
    echo "$!" > .controller.pid
    ./poll-ready.sh http://127.0.0.1:7999/ready 300 || ((./controller.sh stop) && (exit 1))
;;
stop)
  echo "Attempting to stop controller..."
  rm .controller.pid
  killall controller-agent
;;
clear)
  git checkout ../rust/dispatcher-agent/src/decontainerized_functions/mod.rs
;;
*)
  echo "Usage:"
  echo ""
  echo "  controller.sh start"
  echo "  controller.sh stop"
esac
