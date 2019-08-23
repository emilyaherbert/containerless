let containerless = require("../dist/index");

containerless.listen(function(req, resp) {
    let ret = { path: 'pizza' };
    if(req.path === '/ping') {
        ret.path = 'pong';
        resp(ret);
    } else {
        ret.path = 'ping';
        resp(ret);
    }
});