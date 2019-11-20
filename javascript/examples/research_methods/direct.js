let containerless = require('../dist/index');

function add(x, i) {
    if(i === 0) {
        return x;
    } else {
        return 1 + add(x, i-1);
    }
}

containerless.listen(function(req) {
    let n = req.body.n;
    let x = add(0, n);
    containerless.respond("Done!");
});
