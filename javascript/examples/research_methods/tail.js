let containerless = require('containerless');

function add(x, i) {
    if(i === 0) {
        return x;
    } else {
        add(x+1, i-1);
    }
}

containerless.listen(function(req) {
    let n = req.body.n;
    let x = add(0, n);
    containerless.respond("Done!");
});
