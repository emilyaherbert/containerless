set -x
set -e

#export RUST_SRC_PATH=$(dirname $(dirname $(realpath $0)))/rust

#echo "Clearing system state..."
#cp "${RUST_SRC_PATH}/dispatcher-agent/src/decontainerized_functions/template.txt" "${RUST_SRC_PATH}/dispatcher-agent/src/decontainerized_functions/mod.rs"
#rm "${RUST_SRC_PATH}/dispatcher-agent/src/decontainerized_functions/*.json" 2> /dev/null || true
#rm "${RUST_SRC_PATH}/dispatcher-agent/src/decontainerized_functions/*.rs" 2> /dev/null || true
#echo "System state cleared.\n"

(cd javascript/containerless && yarn install && yarn run build)
(cd javascript/js-transform && yarn install && yarn run build)
(cd rust && cargo build --release)
#(cd rust/dispatcher-agent && cargo build)
#(cd rust/function-runner-agent && cargo build)
#(cd rust/function-storage-agent && cargo build)
(cd rust/local && cargo build --release)
(cd docker && make)
