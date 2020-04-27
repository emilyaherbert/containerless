#!/bin/bash
case $1 in
start)
    if [ -f .controller.pid ]; then
        echo "Controller already running"
        exit 1
    fi

    cd ../rust
    RUST_LOG=error,controller=debug ./target/debug/controller-agent &> ../docker/controller.log &
    cd ../docker
    echo "$!" > .controller.pid
    ./poll-ready.sh http://127.0.0.1:7999/ready 300 || ((./controller.sh stop) && (exit 1))
;;
stop)
  kill `cat .controller.pid`; rm .controller.pid
;;
logs)
  tail -f controller.log
;;
clear)
  git checkout ../rust/dispatcher-agent/src/decontainerized_functions/mod.rs
;;
*)
  echo "Usage:"
  echo ""
  echo "  controller.sh start"
  echo "  controller.sh stop"
  echo "  controller.sh logs"
esac
