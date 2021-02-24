let containerless = require('containerless');
containerless.listen(function(req) {
    function makeAdder(x) {
        return function(y) {
            x = x + 1; // NOTE: Crazy
            return x + y;
        }
    }

    let crazy = makeAdder(10);
    let result = crazy(1) + crazy(2); // (11 + 1) + (12 + 2) === 26

    if (result === 26) {
        containerless.respond("ok");
    }
    else {
        containerless.respond("error");
    }
});