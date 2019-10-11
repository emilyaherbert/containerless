let containerless = require('../dist/index');

// https://cloud.google.com/datastore/docs/reference/data/rest/v1/projects/commit

// 1. gcloud auth activate-service-account --key-file keys/umass-plasma-980eb8bee293.json
// 2. gcloud auth print-access-token

/*
Cannot use functions in object
*/

let transaction = undefined;
let mutations = [];

function checkTransaction(next, req) {
    if(transaction === undefined) {
        containerless.respond("You must begin a transaction first.");
    } else {
        next(req);
    }
}

function begin(req) {
    containerless.post({
        'url':"https://datastore.googleapis.com/v1/projects/umass-plasma:beginTransaction?access_token=" + req.query.accessToken,
        'body':{
            "transactionOptions": {
              "readWrite": {
                "previousTransaction": ""
              }
            }
          }
    }, function(resp) {
        transaction = resp.transaction;
        containerless.respond("Begin transaction. Commit transaction with /commit.");
    });
}

function commit(req) {
    containerless.post({
        'url':'https://datastore.googleapis.com/v1/projects/umass-plasma:commit?access_token=' + req.query.accessToken,
        'body': {
            "transaction": transaction,
            "mode": "TRANSACTIONAL",
            "mutations": mutations
          }
    }, function(resp) {
        transaction = undefined;
        console.log(resp);
        containerless.respond("Done!");
    });
}

function balance(req) {
    containerless.post({
        'url':'https://datastore.googleapis.com/v1/projects/umass-plasma:lookup?access_token=' + req.query.accessToken,
        'body': {
            "readOptions": {
              "transaction": transaction
            },
            "keys": [
              {
                "partitionId": {
                  "namespaceId": "",
                  "projectId": "umass-plasma"
                },
                "path": [
                  {
                    "id": 5638535449149440,
                    "kind":"Account"
                  }
                ]
              }
            ]
          }
    }, function(resp) {
        containerless.respond(resp.found[0].entity.properties.Balance.integerValue);
    });
}

function withdraw(req) {
    containerless.post({
        'url':'https://datastore.googleapis.com/v1/projects/umass-plasma:lookup?access_token=' + req.query.accessToken,
        'body': {
            "readOptions": {
              "transaction": transaction
            },
            "keys": [
              {
                "partitionId": {
                  "namespaceId": "",
                  "projectId": "umass-plasma"
                },
                "path": [
                  {
                    "id": 5638535449149440,
                    "kind":"Account"
                  }
                ]
              }
            ]
          }
    }, function(resp) {
        console.log(resp);
        let key = resp.found[0].entity.key;
        let baseVersion = resp.found[0].version;
        let props = resp.found[0].entity.properties;
        props.Balance.integerValue = props.Balance.integerValue - req.query.amount;
        containerless.post({
            'url':'https://datastore.googleapis.com/v1/projects/umass-plasma:commit?access_token=' + req.query.accessToken,
            'body': {
                "transaction": transaction,
                "mode": "TRANSACTIONAL",
                "mutations": [
                    {
                        'update': {
                            'key': key,
                            'properties': props
                        },
                        'baseVersion': baseVersion
                    }
                ]
              }
        }, function(resp2) {
            containerless.respond(resp2);
        });
    });
}

function deposit(req) {
    containerless.respond("TODO!");
}

containerless.listen(function(req) {
    if(req.path === '/begin') {
        begin(req);
    } else if(req.path === '/commit') {
        checkTransaction(commit, req);
    } else if(req.path === '/balance') {
        checkTransaction(balance, req);
    } else if(req.path === '/withdraw') {
        checkTransaction(withdraw, req);
    } else if(req.path === '/deposit') {
        checkTransaction(deposit, req);
    } else {
        containerless.respond("Unknown command.");
    }
});