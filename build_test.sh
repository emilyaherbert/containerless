set -x
set -e
if [ -d /workspace ]; then
  cd /workspace
fi
cd javascript/containerless
yarn install
yarn run build
yarn run test
cd ../js-transform
yarn install
yarn run build
cd ../../rust/compiler
cargo build
cargo test
cd ../invoker
cargo build
cargo test
cd ../shared
cargo build
cargo test
cd ../trace-runtime
cargo build
cargo test
cd ../function-runner-agent
cargo build
cargo test
cd ../function-storage-agent
cargo build
cargo test
