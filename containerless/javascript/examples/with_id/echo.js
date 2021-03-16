let containerless = require('containerless');

containerless.listen(function(req) {
    containerless.respondWithID(req.path, req.query.requestID);
});