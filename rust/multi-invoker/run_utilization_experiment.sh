#!/bin/bash

# $1 run type (Decontainerized, DisableTracing)
# $2 number of open connections
# $3 name

RUN_TYPE=$1
PER_SEC="$2"

# Run the invoker
OUTPUT_PATH="./results/wrk2/${RUN_TYPE}"
mkdir --parents $OUTPUT_PATH
CONFIG1="{\"bind_port\":8081,\"image_name\":\"serverless-function\",\"max_containers\":5,\"initial_state\":\"Tracing\",\"utilization_log\":\"${OUTPUT_PATH}_C/${PER_SEC}.log\",\"kill_parent\":true}"
CONFIG2="{\"bind_port\":8082,\"image_name\":\"serverless-function\",\"max_containers\":6,\"initial_state\":\"Decontainerized\",\"utilization_log\":\"${OUTPUT_PATH}_D/${PER_SEC}.log\",\"kill_parent\":true}"
CONFIG="{\"bind_port\":8080,\"config_a\":${CONFIG1},\"config_b\":${CONFIG2}}"
cargo run --release -- --config ${CONFIG}
