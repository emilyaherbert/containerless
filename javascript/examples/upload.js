let containerless = require('containerless');

containerless.listen(function(req) {
    if(req.path === '/upload') {
        containerless.post({
            url: 'http://10.200.0.1:7999/upload',
            body: req.body
        }, function(resp) {
            containerless.respond(resp.body);
        });
    } else {
        containerless.respond("Unknown command.\n");
    }
});