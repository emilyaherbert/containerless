let containerless = require('../dist/index');

containerless.listen(function(req) {
    // TODO(emily): Fix so that you can chain member expressions.
    let body = req.body;
    if(req.path === '/login') {
        containerless.get("https://emilyaherbert.github.io/authorize.json", function(resp) {
            if(resp.username === undefined || resp.password === undefined || body.username === undefined || body.password === undefined) {
                containerless.respond("Username and password not found.\n");
            } else if(resp.username === body.username && resp.password === body.password) {
                containerless.respond("Login successful!\n");
            } else {
                containerless.respond("Invalid username or password.\n");
            }
        });
    } else {
        containerless.respond("Unknown command.\n");
    }
});