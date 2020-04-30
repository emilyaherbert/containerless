#!/bin/sh
if [ "$#" -ne 2 ]; then
    echo "Usage:"
    echo ""
    echo "  start-lone-function.sh NAME MODE"
    echo ""
    echo "MODE must be either 'tracing' or 'vanilla'."
    echo "There must be file called NAME.js in the ../examples directory."
    exit 1
fi

set -e
export NAME=$1
export MODE=$2
echo $NAME
envsubst < lone-template.yaml | microk8s.kubectl apply -f -
./poll-ready.sh http://localhost/lone-$MODE-$NAME/readinessProbe 10
echo "You can now invoke the function:"
echo ""
echo "  curl -H 'application/json' -X POST -d DATA http://localhost/lone-$MODE-$NAME/PATH"

if [ "$MODE" = "tracing" ]; then
  echo ""
  echo "To extract a trace:"
  echo ""
  echo "curl http://localhost/lone-$MODE-$NAME/trace | python -m json.tool > trace.json"
fi

