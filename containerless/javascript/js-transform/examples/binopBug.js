let containerless = require("../../javascript/containerless");

function same(b) {
    return same;
}

containerless.listen(function(req) {
    if(same(true) && same(false)) {
        containerless.respond("bad path");
    } else {
        containerless.respond("good path");
    }
});