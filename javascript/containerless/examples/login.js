let containerless = require('../dist/index');

containerless.listen(function(req) {
    // TODO(emily): Fix so that you can chain member expressions.
    let query = req.query;
    if(req.path === '/login') {
        containerless.get("https://emilyaherbert.github.io/authorize.json", function(resp) {
            if(resp.username === undefined || resp.password === undefined) {
                containerless.respond("Username and password not found.\n");
            } else if(resp.username === query.username && resp.password === query.password) {
                containerless.respond("Login successful!\n");
            } else {
                containerless.respond("Invalid username or password.\n");
            }
        });
    } else {
        containerless.respond("Unknown command.\n");
    }
});