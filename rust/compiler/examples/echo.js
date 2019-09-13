let containerless = require("../../javascript/containerless");
containerless.listen(function(req, resp) {
    resp('Hello, world!');
});