set -x
set -e
(cd javascript/containerless && yarn install && yarn run build)
(cd javascript/js-transform && yarn install && yarn run build)
(cd rust/compiler && cargo build)
(cd rust/dispatcher-agent && cargo build)
(cd rust/function-runner-agent && cargo build)
(cd rust/function-storage-agent && cargo build)
(cd rust/invoker && cargo build)
(cd rust/local && cargo build)
(cd rust/multi-invoker && cargo build)
(cd docker && make)
