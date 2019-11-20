let containerless = require('containerless');

containerless.listen(function(req) {
    let n = req.body.n;
    let x = 0;
    while(n > 0) {
        x = x + 1;
        n = n - 1;
    }
    containerless.respond("Done!");
});
