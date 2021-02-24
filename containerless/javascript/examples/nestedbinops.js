let containerless = require("containerless");
containerless.listen(function(req) {
    let x = 12;
    if(x > 2 && x < 15) {
        containerless.respond("yay!");
    } else {
        containerless.respond("boo!");
    }
});