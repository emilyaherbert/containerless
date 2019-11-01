let containerless = require('containerless');

containerless.listen(function(req) {
    if(req.path === '/status') {
        if (typeof req.body.username !== 'string') {
            containerless.respond(req.body + "\n");
            containerless.respond('Username missing');
            return;
        }
        if (typeof req.body.token !== 'string') {
            containerless.respond('Token missing');
            return;
        }
        if (typeof req.body.state !== 'string') {
            containerless.respond('State missing');
            return;
        }

        containerless.get('http://localhost:7999/status',
          function(resp1) {
            if (typeof resp1.commit === 'object' && typeof resp1.commit.sha !== 'string') {
                containerless.respond('SHA missing in resp1');
                return;
            } else if (typeof resp1.body !== 'undefined') {
                containerless.respond(resp1.body);
                return;
            }
            containerless.post({
                'url': 'http://localhost:7999/status',
                'headers': {
                    'User-Agent': req.body.username
                },
                'auth': {
                    'username': req.body.username,
                    'password': req.body.token
                },
                'body': {
                    'state': req.body.state
                }
            }, function(resp2) {
                containerless.respond(resp2.body);
            });
        });
    } else {
        containerless.respond("Unknown command.");
    }
});