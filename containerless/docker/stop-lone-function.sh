#!/bin/sh
if [ "$#" -ne 2 ]; then
    echo "Usage:"
    echo ""
    echo "  stop-lone-function.sh NAME MODE"
    echo ""
    echo "MODE must be either 'tracing' or 'vanilla'."
    exit 1
fi

set -e
export NAME=$1
export MODE=$2
envsubst < lone-template.yaml | microk8s.kubectl delete --ignore-not-found -f -
