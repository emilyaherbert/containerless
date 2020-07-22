let containerless = require('../dist/index');

containerless.listen(function(req) {
    let x = req.query.x;
    if(x > 10) {
        containerless.respond("42");
        return;
    }
    containerless.respond("24");
});