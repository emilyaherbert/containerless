let containerless = require('containerless');

containerless.listen(function(req) {
    if(req.path === '/status') {
        /*
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
        */

        containerless.post({ 'url': 'http://10.200.0.1:7999/status1' },
          function(resp1) {
            if (typeof resp1.commit.sha !== 'string') {
                containerless.respond('SHA missing in resp1');
                return;
            }
            containerless.respond("spaghetti");
            /*
            containerless.post({
                'url': 'http://10.200.0.1:7999/status2',
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
            */
        });
    } else {
        containerless.respond("Unknown command.");
    }
});