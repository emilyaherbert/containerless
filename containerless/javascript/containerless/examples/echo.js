let containerless = require("../dist/index");

containerless.listen(function(req) {
    containerless.respond(req.path);
});