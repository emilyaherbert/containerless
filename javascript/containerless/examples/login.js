let containerless = require('../dist/index');

containerless.listen(function(req, resp) {
    if(req.path === '/login') {
        containerless.get("https://emilyaherbert.github.io/authorize.txt", function(response) {
            resp("Someone tried to log in.");
            /*
            if(response.username === 'javascript' && response.password === 'rust') {
                resp("Login successful!\n");
            } else {
                resp("Invalid username or password.\n");
            }
            */
        });
    } else {
        resp(req);
    }
});