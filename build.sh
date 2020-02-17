set -x
set -e
if [ -d /workspace ]; then
  cd /workspace
fi
cd javascript/containerless
yarn install
yarn run build
cd ../js-transform
yarn install
yarn run build
cd ../../rust/compiler
cargo build
cd ../invoker
cargo build
cd ../shared
cargo build
cd ../trace-runtime
cargo build
cd ../function-runner-agent
cargo build
cd ../function-storage-agent
cargo build
