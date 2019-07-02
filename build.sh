set -x
set -e
cd /workspace/containerless
yarn install
yarn run build
yarn run test