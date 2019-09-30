#!/bin/bash

# set -x

if [ "$#" -ne 1 ]; then
    echo "Usage:"
    echo "  ./prepare_serverless_function.sh function.js"

    exit 1
fi


set -e

cp $1 `dirname $0`/workspace_prepare_serverless_function/pkg/index.js
cd `dirname $0`

rsync -ad ../javascript/containerless workspace_prepare_serverless_function/pkg/node_modules
cd workspace_prepare_serverless_function
docker build -t serverless-function .