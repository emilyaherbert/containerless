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
cargo build --release
cd ../invoker
cargo build --release
cd ../multi-invoker
cargo build --release
cd ../shared
cargo build --release
cd ../function-runner-agent
cargo build --release
cd ../function-storage-agent
cargo build --release
