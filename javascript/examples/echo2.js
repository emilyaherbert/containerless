let containerless = require('containerless');

containerless.listen(function(req) {
    containerless.respond(req.body.amount);
});
