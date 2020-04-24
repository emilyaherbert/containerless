#!/bin/sh
set -e

curl --output /dev/null --silent --fail -X POST http://localhost/controller/reset_dispatcher
./poll-ready.sh http://localhost/controller/ok_if_not_compiling 60
sleep 2
echo -n "Waiting until exactly one replica "
REPLICAS=$(microk8s.kubectl get deployment/dispatcher -n containerless -o json | jq ".status.replicas")
until [ "$REPLICAS" -eq "1" ]; do
    echo -n "."
    REPLICAS=$(microk8s.kubectl get deployment/dispatcher -n containerless -o json | jq ".status.replicas")
    sleep 1
done
echo "OK"

(cd ../rust/integration-tests && cargo test -- --test-threads=1)
