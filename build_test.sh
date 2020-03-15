set -x
set -e
(cd javascript/containerless && yarn install && yarn run build && yarn run test)
(cd rust/compiler && cargo build && cargo test)