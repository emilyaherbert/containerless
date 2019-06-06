https://cloud.google.com/run/docs/quickstarts/build-and-deploy?authuser=1
https://cloud.google.com/run/docs/building/containers?authuser=1
https://github.com/knative/docs/tree/175313457f94baa036b400f12d162157edef70a7/community/samples/serving/helloworld-rust


# Steps

1. `cargo build`
2. `gcloud builds submit --tag gcr.io/umass-plasma/helloworld --project umass-plasma`
3. `gcloud beta run deploy --image gcr.io/umass-plasma/helloworld --project umass-plasma`