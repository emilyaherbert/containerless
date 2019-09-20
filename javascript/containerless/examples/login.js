let containerless = require('../dist/index');

containerless.listen(function(req) {
    if(req.path === '/login') {
        containerless.get("https://emilyaherbert.github.io/authorize.json", function(resp) {
            /*
            if(resp.username === 'javascript' && resp.password === 'rust') {
                containerless.respond("Found a user in the database!\n");
            } else {
                containerless.respond("No user found.\n");
            }
            */
            if(resp.username === undefined || resp.password === undefined) {
                containerless.respond("Username and password not found.\n");
            } else if(resp.username === 'javascript' && resp.password === 'rust') {
                containerless.respond("Login successful!\n");
            } else {
                containerless.respond("Invalid username or password.\n");
            }
        });
    } else {
        containerless.respond("Unknown command.\n");
    }
});