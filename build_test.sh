set -x
set -e
(cd javascript/containerless && yarn install && yarn run build && yarn run test)
(cd javascript/js-transform && yarn install && yarn run build)
(cd rust/compiler && cargo build && cargo test)
(cd rust/invoker && cargo build &&)
(cd rust/multi-invoker && cargo build &&)
(cd rust/shared && cargo build &&)