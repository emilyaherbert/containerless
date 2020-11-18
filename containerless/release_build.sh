set -x
set -e
(cd javascript/containerless && yarn install && yarn run build)
(cd javascript/js-transform && yarn install && yarn run build)
(cd rust && cargo build --release)
(cd docker && make release)
