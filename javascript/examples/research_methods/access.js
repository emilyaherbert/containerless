let containerless = require('containerless');

containerless.listen(function(req) {
    let n = req.body.n;
    let x = [];
    for(let i=0; i<100000; i++) {
        x.push(0);
    }
    for(let i=0; i<n; i++) {
        x[i];
    }
    containerless.respond("Done!");
});
