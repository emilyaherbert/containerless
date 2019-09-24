1. `./node_modules/js-transform/dist/index.js INPUT_FILE > TRANSFORMED_FILE`
2. `node TRANSFORMED_FILE PORT`

POST:
> curl -H "Content-Type: application/json" -d @files/credentials.json localhost:8080/login

GET:
> curl localhost:8080/trace