1. `./node_modules/js-transform/dist/index.js INPUT_FILE > TRANSFORMED_FILE`
2. `node TRANSFORMED_FILE PORT`

POST:
`curl -H "Content-Type: application/json" -d @input/login.json localhost:8080/login`
`containerless$ curl -H "Content-Type: text/plain" --data-binary @input/upload.txt localhost:8080/upload`

GET:
`curl localhost:8080/trace`