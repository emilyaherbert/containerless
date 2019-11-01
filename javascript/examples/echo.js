let containerless = require('containerless');

containerless.listen(function(req) {
    console.error(req.path);
    containerless.respond(req.path);
});
