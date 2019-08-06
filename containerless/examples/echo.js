let containerless = require('../dist/index');

containerless.listen(function(req, resp) {
    console.log('Got a response');
    resp(req);
});