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
