[https://github.com/GoogleCloudPlatform/nodejs-docs-samples/tree/master/functions/datastore]


## Deploy
        gcloud functions deploy register --trigger-http --project umass-plasma --runtime nodejs8
        gcloud functions deploy login    --trigger-http --project umass-plasma --runtime nodejs8
        gcloud functions deploy remove   --trigger-http --project umass-plasma --runtime nodejs8

## Test
        gcloud functions call register --project umass-plasma --data '{"username":"emily", "password":"herbert"}'
        gcloud functions call login    --project umass-plasma --data '{"username":"emily", "password":"herbert"}'
        gcloud functions call remove   --project umass-plasma --data '{"username":"emily", "password":"herbert"}'