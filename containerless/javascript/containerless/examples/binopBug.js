let containerless = require('../dist/index');

function same(b) {
    return b;
}

containerless.listen(function(req) {
    if(same(true) && same(false)) {
        containerless.respond("bad path");
    } else {
        containerless.respond("good path");
    }
});