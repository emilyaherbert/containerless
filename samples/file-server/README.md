# File Server

This sample application interacts with a database (CouchDB) to host a file
server.

## Deploying

### Deploy CouchDB to Kubernetes

1. Install Helm

2. Add CouchDB to Helm

    ```
    $ helm repo add couchdb https://apache.github.io/couchdb-helm
    ```

3. Create a UUID

    ```
    $ curl https://www.uuidgenerator.net/api/version4 2>/dev/null | tr -d -
    ```

4. Install the Helm chart using the `<uuid>` creating in step 3

    ```
    $ helm install my-release couchdb/couchdb \
        --set couchdbConfig.couchdb.uuid=<uuid>
    ```

    This will start a CouchDB pod inside of the Kubernetes cluster and will
    start a ClusterIP service that listens on port 5984 and that allows you to
    interact with CouchDB from inside of the CouchDB pod.

    A set of "next instructions" will be printed on screen, but we don't want to
    execute those quite yet...

5. Retrieve the CouchDB admin credentials

    ```
    $ k get secret my-release-couchdb \
        -o go-template='{{ .data.adminUsername }}' | base64 --decode
    $ k get secret my-release-couchdb \
        -o go-template='{{ .data.adminPassword }}' | base64 --decode
    ```

6. Follow the instructions printed after step 4, using the credentials retrieved
    in step 5

7. Deploy a Kubernetes service so that the CouchDB pod can be accessed from
    outside of the Kubernetes cluster

    ```
    $ k create -f services.yaml
    ```

8. Create the database for the sample application using the `<username>` and
    `<password>` retrieved from step 5

    ```
    $ curl -X PUT http://<username>:<password>@localhost:30984/myfiles
    ```

### Deploy the file server to Containerless

Create the file server serverless function:

```
$ containerless create -n fileserver -f fileServer.js
```

## Invoking

Invoke using the `<username>` and `<password>` from step 5 above:

```
$ curl -X POST -H "Content-Type: application/json" "http://localhost/dispatcher/fileserver/upload?username=<username>&password=<password>&filename=grocerylist" -d @groceries.json
```

## Undeploying

```
$ containerless delete -n fileserver
```