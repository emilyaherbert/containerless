let containerless = require("../../javascript/containerless");

function F(x) {
    if(x > 10) {
        return 42;
    } else {
        return 24;
    }
}

containerless.listen(function(req) {
    let x = 12;
    for(let i=0; i<10; i++) {
        x = x - 1;
    }
    if(x === 2) {
        containerless.respond("yay!");
    } else {
        containerless.respond("boo!");
    }
});