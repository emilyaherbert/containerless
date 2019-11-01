let containerless = require('containerless');

containerless.listen(function(req) {
    if(req.path === '/login') {
	let u = req.body.username;
	let p = req.body.password;
	if(u === undefined || p === undefined) {
	    containerless.respond("Username or password not found.\n")
	} else {
            containerless.post({
                    url: 'http://10.200.0.1:7999/login',
                    body: req.body
                }, function(resp) {
                if(resp.username === undefined || resp.password === undefined) {
                    containerless.respond(resp.body);
                } else if(resp.username === u && resp.password === p) {
                    containerless.respond("Login successful!");
                } else {
                    containerless.respond(resp.body);
                }
            });
	}
    } else {
        containerless.respond("Unknown command.");
    }
});
