let containerless = require('../dist/index');

containerless.listen(function(req, resp) {
    if(req.path === '/ping') {
        resp({ path: 'pong' });
    } else {
        resp({ path: 'ping' });
    }
});