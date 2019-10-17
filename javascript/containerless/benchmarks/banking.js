let containerless = require('../dist/index');

// https://cloud.google.com/datastore/docs/reference/data/rest/v1/projects/commit

// 1. gcloud auth activate-service-account --key-file keys/umass-plasma-980eb8bee293.json
// 2. gcloud auth print-access-token

// TODO(emily): Fix bug where you can't call functions from inside objects.

/**
 * Begins a request.
 * 
 * `next(req, transaction)`
 */
function begin(req, next) {
    containerless.post({
        'url':"https://datastore.googleapis.com/v1/projects/umass-plasma:beginTransaction?access_token=" + req.body.accessToken,
        'body':{
            "transactionOptions": {
              "readWrite": {
                "previousTransaction": ""
              }
            }
          }
    }, function(resp) {
        if(resp.error !== undefined) {
            containerless.respond("error\n");
        } else {
            next(req, resp.transaction);
        }
    });
}

/**
 * Commits a request.
 */
function commit(req, transaction, mutation) {
    containerless.post({
        'url':'https://datastore.googleapis.com/v1/projects/umass-plasma:commit?access_token=' + req.body.accessToken,
        'body': {
            "transaction": transaction,
            "mode": "TRANSACTIONAL",
            "mutations": [mutation]
          }
    }, function(resp) {
        if(resp.error !== undefined) {
            containerless.respond("error\n");
        } else {
            containerless.respond("Done!\n");
        }
    });
}

/**
 * Finds the current balance.
 * 
 * `next(resp);`
 */
function balance(req, transaction, next) {
    containerless.post({
        'url':'https://datastore.googleapis.com/v1/projects/umass-plasma:lookup?access_token=' + req.body.accessToken,
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
                    "id": req.body.id,
                    "kind":"Account"
                  }
                ]
              }
            ]
          }
    }, function(resp) {
        if(resp.error !== undefined) {
            containerless.respond("error\n");
        } else {
            next(resp);
        }
    });
}

/**
 * Performs a withdrawl.
 */
function withdraw(req, transaction) {
    balance(req, transaction, function(resp) {
        let key = resp.found[0].entity.key;
        let baseVersion = resp.found[0].version;
        let props = resp.found[0].entity.properties;
        props.Balance.integerValue = props.Balance.integerValue - req.query.amount;
        commit(req, transaction,
            {
            'update': {
                'key': key,
                'properties': props
            },
            'baseVersion': baseVersion
        });
    });
}

/**
 * Performs a deposit.
 */
function deposit(req, transaction) {
    balance(req, transaction, function(resp) {
        let key = resp.found[0].entity.key;
        let baseVersion = resp.found[0].version;
        let props = resp.found[0].entity.properties;
        props.Balance.integerValue = props.Balance.integerValue - (req.query.amount - (req.query.amount * 2));
        commit(req, transaction,
            {
                'update': {
                    'key': key,
                    'properties': props
                },
                'baseVersion': baseVersion
            });
    });
}

containerless.listen(function(req) {
    if(req.path === '/balance') {
        begin(req, function(req, transaction) {
            balance(req, transaction, function(resp) {
                containerless.respond(resp.found[0].entity.properties.Balance.integerValue + '\n');
            });
        });
    } else if(req.path === '/withdraw') {
        begin(req, withdraw);
    } else if(req.path === '/deposit') {
        begin(req, deposit);
    } else {
        containerless.respond("Unknown command.\n");
    }
});