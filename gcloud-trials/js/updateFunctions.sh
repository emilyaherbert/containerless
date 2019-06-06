#!/bin/bash
# Shell script to emulate/deploy all Cloud Functions in the file

gcloud functions deploy register --trigger-http --project umass-plasma --runtime nodejs8
echo '-----------------------------'
gcloud functions deploy login --trigger-http --project umass-plasma --runtime nodejs8
echo '-----------------------------'
gcloud functions deploy remove --trigger-http --project umass-plasma --runtime nodejs8
echo '-----------------------------'
gcloud functions deploy list --trigger-http --project umass-plasma --runtime nodejs8
echo '-----------------------------'
gcloud functions deploy getFile --trigger-http --project umass-plasma --runtime nodejs8
