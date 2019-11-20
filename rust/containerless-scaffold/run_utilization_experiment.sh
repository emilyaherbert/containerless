#!/bin/bash

# $1 run type (Decontainerized, DisableTracing)
# $2 number of open connections
# $3 name

RUN_TYPE=$1
OPEN_CONNECTIONS="$2"
PER_SEC="$3"

# Run the invoker
OUTPUT_PATH="./results/wrk2/${OPEN_CONNECTIONS}/${RUN_TYPE}"
mkdir --parents $OUTPUT_PATH
CONFIG="{\"image_name\":\"serverless-function\",\"max_containers\":6,\"initial_state\":\"${RUN_TYPE}\",\"utilization_log\":\"${OUTPUT_PATH}/${PER_SEC}.log\"}"
cargo run --release -- --config ${CONFIG}
