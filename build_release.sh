set -x
set -e
(cd javascript/containerless && yarn install && yarn run build)
(cd javascript/js-transform && yarn install && yarn run build)
(cd rust/compiler && cargo build --release)
(cd rust/invoker && cargo build --release)
(cd rust/multi-invoker && cargo build --release)
(cd rust/shared && cargo build --release)