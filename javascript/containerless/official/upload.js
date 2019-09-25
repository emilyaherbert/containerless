let containerless = require('../dist/index');

containerless.listen(function(req) {
    let file = req.file;
    if(req.path === '/upload') {
        containerless.post({
            url: 'http://localhost:3000/upload',
            method: 'POST',
            body: "fdasfdsafsd"
        }, function(resp) {
            containerless.respond(resp);
        });
    } else {
        containerless.respond("Unknown command.\n");
    }
});