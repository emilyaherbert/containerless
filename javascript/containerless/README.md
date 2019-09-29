1. `./node_modules/js-transform/dist/index.js INPUT_FILE > TRANSFORMED_FILE`
2. `node TRANSFORMED_FILE PORT`

POST:
`curl -X POST -H "Content-Type: application/json" -d @input/login.json localhost:8080/login`
`curl -X POST -H "Content-Type: application/json" -d @input/upload.json localhost:8080/upload`
`curl -X POST -H "Content-Type: application/json" -d @input/token_status.json localhost:8080/status?state=success`

GET:
`curl localhost:8080/trace`