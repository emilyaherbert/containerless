let containerless = require('containerless');

containerless.listen(function(req) {
    if(req.path === '/status') {
        if (typeof req.body.username !== 'string') {
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

        containerless.get('http://10.200.0.1:7999/status',
          function(resp1) {
	    if (typeof resp1.commit !== 'object') {
		containerless.respond("No commit.");
		return;
	    }
            if (typeof resp1.commit.sha !== 'string') {
                containerless.respond('No SHA.');
		return;
            }
		  
            containerless.post({
                'url': 'http://10.200.0.1:7999/status?sha=' + resp1.commit.sha,
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
