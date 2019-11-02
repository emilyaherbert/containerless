let containerless = require('containerless');

/**
 * Begins a request.
 *
 * `next(req, transaction)`
 */
function begin(req, next) {
    containerless.get('http://10.200.0.1:7999/begin', function(resp) {
        if(resp === undefined) {
            console.log(resp);
            containerless.respond("No response. 1");
            return;
        }
        if(resp.transaction === undefined) {
            containerless.respond("No transaction 1.");
            return;
        }
        next(req, resp.transaction);
    });
}

/**
 * Commits a request.
 */
function commit(req, transaction, mutation) {
    let o = {
        'url': 'http://10.200.0.1:7999/commit',
        'body': {
            "transaction": transaction,
            "mutation": mutation
        }
    };
    containerless.post(o, function(resp) {
        if(resp === undefined) {
            containerless.respond("No response 2.");
            return;
        }
        if(resp.body !== undefined) {
            containerless.respond(resp.body);
            return;
        }
        containerless.respond("Something happened but not sure what.");
    });
}

/**
 * Finds the current balance.
 *
 * `next(resp);`
 */
function balance(req, transaction, next) {
    let o = {
        'url': 'http://10.200.0.1:7999/balance',
        'body': {
            'name': req.body.account
        }
    };
    containerless.post(o, function(resp) {
        if(resp === undefined) {
            containerless.respond("No response 3.");
            return;
        }
        next(resp);
    });
}

/**
 * Performs a withdrawl.
 */
function withdraw(req, transaction) {
    balance(req, transaction, function(resp) {
        if(resp === undefined) {
            containerless.respond("No response 4.");
            return;
        }
        if(resp.balance === undefined) {
            containerless.respond("No balance 4.");
            return;
        }
        let newBalance = resp.balance - (100 * req.query.amount);
        let o = {
            'name': req.body.account,
            'balance': newBalance
        };
        commit(req, transaction, o);
    });
}

/**
 * Performs a deposit.
 */
function deposit(req, transaction) {
    balance(req, transaction, function(resp) {
        if(resp === undefined) {
            containerless.respond("No response 5.");
            return;
        }
        if(resp.balance === undefined) {
            containerless.respond("No balance 5.");
            return;
        }
        let newBalance = resp.balance - (-100 * req.query.amount);
        let o = {
            'name': req.body.account,
            'balance': newBalance
        };
        commit(req, transaction, o);
    });
}

containerless.listen(function(req) {
    if(req.body === undefined) {
        containerless.respond("Undefined body.");
        return;
    }
    if(req.body.account === undefined) {
        containerless.respond("Account undefined.");
        return;
    }
    if(req.path === '/balance') {
        begin(req, function(req, transaction) {
            balance(req, transaction, function(resp) {
                if(resp.balance === undefined) {
                    containerless.respond("No balance 6.");
                    return;
                }
                containerless.respond(resp.balance / 100.0);
            });
        });
    } else if(req.path === '/withdraw') {
        if(req.query === undefined) {
            containerless.respond("Query undefined 1.");
            return;
        }
        if(req.query.amount === undefined) {
            containerless.respond("Amount undefined 1.");
            return;
        }
        begin(req, withdraw);
    } else if(req.path === '/deposit') {
        if(req.query === undefined) {
            containerless.respond("Query undefined 2.");
            return;
        }
        if(req.query.amount === undefined) {
            containerless.respond("Amount undefined 2.");
            return;
        }
        begin(req, deposit);
    } else {
        containerless.respond("Unknown command.\n");
    }
});