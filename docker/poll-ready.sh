#!/bin/sh

if [ "$#" -ne 2 ]; then
    echo "Usage: poll-ready.sh URL NUMBER-OF-ATTEMPTS"
fi

URL=$1
MAX_ATTEMPTS=$2
COUNTER=0

echo -n "Probing $URL "
# Ideally, we would check that the HTTP response has status code 200, but the
# test below is more permissive than that. However, curl appears to produce
# a non-zero exit code on the 5xx errors that K8s produces when a
# service/ingress is unreacahble, which is what matters for us.
until $(curl --output /dev/null --silent --fail $URL); do
    if [ ${COUNTER} -eq ${MAX_ATTEMPTS} ]; then
        echo " FAILED."
        exit 1
    fi
    COUNTER=$(($COUNTER+1))
    echo -n '.'
    sleep 1
done

echo "OK."