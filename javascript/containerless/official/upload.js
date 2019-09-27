let containerless = require('../dist/index');

containerless.listen(function(req) {
    if(req.path === '/upload') {
        if(req.body !== undefined) {
            containerless.post({
                url: 'http://localhost:3000/upload',
                body: req.body
            }, function(resp) {
                containerless.respond(resp);
            });
        } else {
            containerless.respond("No file to upload.\n");
        }
    } else {
        containerless.respond("Unknown command.\n");
    }
});