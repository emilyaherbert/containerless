let containerless = require('../dist/index');

containerless.listen(function(req) {
    if(req.path === '/upload') {
        if(req.body !== undefined) {
            containerless.post({
                url: 'http://localhost:3000/upload',
                body: req.body
            }, function(resp) {
                containerless.respond('Uploaded');
            });
        } else {
            containerless.respond("No file to upload.");
        }
    } else {
        containerless.respond("Unknown command.");
    }
});
