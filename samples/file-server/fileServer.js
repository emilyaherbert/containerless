let containerless = require('containerless');
containerless.listen(function(req) {
    if(req.path === '/upload') {
        if(req.body !== undefined && req.query !== undefined && req.query.username !== undefined && req.query.password !== undefined && req.query.filename !== undefined) {
            let options = {
                url: 'http://' + req.query.username + ':' + req.query.password + '@10.200.0.28:30984/myfiles/"' + req.query.filename + '"',
                body: req.body
            }
            containerless.put(options, function(resp) {
                if(resp !== undefined && resp.body !== undefined) {
                    containerless.respond(resp.body);
                } else if(resp.error !== undefined) {
                    containerless.respond(resp.error);
                } else {
                    containerless.respond(resp);
                }
            });
        } else {
            containerless.respond("Malformed request.\n");
        }
    } else {
        containerless.respond("Unknown command.\n");
    }
});