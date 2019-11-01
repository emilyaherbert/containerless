let containerless = require('containerless');

containerless.listen(function(req) {
    if(req.path === '/login') {
        containerless.post({
                url: 'http://10.200.0.1:7999/login',
                body: req.body
            }, function(resp) {
            if(resp.username === undefined || resp.password === undefined || req.body.username === undefined || req.body.password === undefined) {
                console.error(resp);
                containerless.respond("Username and password not found.");
            } else if(resp.username === req.body.username && resp.password === req.body.password) {
                containerless.respond("Login successful!");
            } else {
                containerless.respond(resp + "\n");
            }
        });
    } else {
        containerless.respond("Unknown command.");
    }
});
