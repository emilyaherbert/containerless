#!/bin/bash

# set -x

if [ "$#" -ne 1 ]; then
    echo "Usage:"
    echo "  ./prepare_serverless_function.sh function.js"

    exit 1
fi


set -e

D=workspace_prepare_serverless_function/pkg
cp $1 `dirname $0`/$D/index.js
cd `dirname $0`
node ../javascript/js-transform/dist/index $D/index.js > $D/traced.js

rsync -ad ../javascript/containerless $D/node_modules
cd workspace_prepare_serverless_function
docker build -t serverless-function .