let containerless = require('../dist/index');

containerless.listen(function(req) {
    if(req.path === '/login') {
        containerless.get("https://emilyaherbert.github.io/authorize.json", function(resp) {
            if(resp.username === undefined || resp.password === undefined || req.body.username === undefined || req.body.password === undefined) {
                containerless.respond("Username and password not found.\n");
            } else if(resp.username === req.body.username && resp.password === req.body.password) {
                containerless.respond("Login successful!\n");
            } else {
                containerless.respond("Invalid username or password.\n");
            }
        });
    } else {
        containerless.respond("Unknown command.\n");
    }
});