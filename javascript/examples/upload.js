let containerless = require('containerless');

containerless.listen(function(req) {
    if(req.path === '/upload') {
        if(req.body !== undefined) {
            containerless.post({
                url: 'http://10.200.0.1:7999/upload',
                body: req.body
            }, function(resp) {
                containerless.respond("Uploaded!");
            });
        } else {
            containerless.respond("No file to upload.\n");
        }
    } else {
        containerless.respond("Unknown command.\n");
    }
});