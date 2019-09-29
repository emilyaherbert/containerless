let containerless = require('../dist/index');

containerless.listen(function(req) {
    if(req.path === '/status') {
        containerless.get({
            'url': 'https://api.github.com/repos/plasma-umass/decontainerization/branches/test-status',
            'headers': {
                'User-Agent': req.body.username
            },
            'auth': {
                'username': req.body.username,
                'password': req.body.token
            }
        }, function(resp1) {
            containerless.post({
                'url': 'https://api.github.com/repos/plasma-umass/decontainerization/statuses/' + resp1.commit.sha,
                'headers': {
                    'User-Agent': req.body.username
                },
                'auth': {
                    'username': req.body.username,
                    'password': req.body.token
                },
                'body': {
                    'state': req.query.state
                }
            }, function(resp2) {
                containerless.respond("Done!");
            });
        });
    } else {
        containerless.respond("Unknown command.");
    }
});