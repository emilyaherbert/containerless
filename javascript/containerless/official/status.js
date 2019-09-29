let containerless = require('../dist/index');

// https://developer.github.com/v3/repos/statuses/
// https://api.github.com/repos/plasma-umass/decontainerization
// curl -X GET -u "emilyaherbert:TOKEN" https://api.github.com/repos/plasma-umass/decontainerization

containerless.listen(function(req) {
    if(req.path === '/status') {
        containerless.get({
            url: "https://api.github.com/repos/plasma-umass/decontainerization/branches/test-status",
            headers: {
                'User-Agent': 'request'
            },
            auth: {
                'username': req.body.username,
                'password': req.body.token
            }
        }, function(resp) {
            containerless.respond(resp);
        });
    } else {
        containerless.respond("Unknown command.\n");
    }
});