var decontainerizable = require('../dist/index');

decontainerizable.listen(function(req, resp) {
    if(req.path === '/ping') {
        resp({ path: 'pong' });
    } else {
        resp({ path: 'ping' });
    }
});