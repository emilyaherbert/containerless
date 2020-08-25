let containerless = require('containerless');

/**
 * Begins a request.
 *
 * `next(req, transaction)`
 */
function begin(req, next) {
    containerless.get('http://10.200.0.1:7999/begin', function(resp) {
        if(resp === undefined) {
            containerless.respond("No response. 1");
        } else if(resp.transaction === undefined) {
            containerless.respond("No transaction 1.");
        } else {
            next(req, resp.transaction);
        }
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
        } else if(resp.body !== undefined) {
            containerless.respond(resp.body);
        } else {
            containerless.respond("Something happened but not sure what.");
        }
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
        if(resp === undefined) {
            containerless.respond("No response 4.");
        } else if(resp.balance === undefined) {
            containerless.respond("No balance 4.");
        } else {
            let newBalance = resp.balance - req.body.amount;
            let o = {
                'name': req.body.account,
                'balance': newBalance
            };
            commit(req, transaction, o);
        }
    });
}

/**
 * Performs a deposit.
 */
function deposit(req, transaction) {
    balance(req, transaction, function(resp) {
        if(resp === undefined) {
            containerless.respond("No response 5.");
        } else if(resp.balance === undefined) {
            containerless.respond("No balance 5.");
        } else {
            let newBalance = resp.balance - (-1 * req.body.amount);
            let o = {
                'name': req.body.account,
                'balance': newBalance
            };
            commit(req, transaction, o);
        }
    });
}

containerless.listen(function(req) {
    if(req.body === undefined) {
        containerless.respond("Undefined body.");
    } else if(req.body.account === undefined) {
        containerless.respond("Account undefined.");
    } else if(req.path === '/balance') {
        begin(req, function(req, transaction) {
            balance(req, transaction, function(resp) {
                if(resp.balance === undefined) {
                    containerless.respond("No balance 6.");
                } else {
                    containerless.respond(resp.balance);
                }
            });
        });
    } else if(req.path === '/withdraw') {
        if(req.body.amount === undefined) {
            containerless.respond("Amount undefined 1.");
        } else {
            begin(req, withdraw);
        }
    } else if(req.path === '/deposit') {
        if(req.body.amount === undefined) {
            containerless.respond("Amount undefined 2.");
        } else {
            begin(req, deposit);
        }
    } else {
        containerless.respond("Unknown command.\n");
    }
});