set -x
set -e
cd /workspace/containerless
yarn install
cd /workspace/js-transform
yarn install
yarn link
cd /workspace/containerless
yarn link js-transform
yarn run build
yarn run test
cd /workspace/trace-verif
cargo build
cargo test
