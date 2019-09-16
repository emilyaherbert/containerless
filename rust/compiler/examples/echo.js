let containerless = require("../../javascript/containerless");
containerless.listen(function(req, resp) {
    //console.error('Got a response');
    resp(req);
});