let containerless = require("containerless");
containerless.listen(function(req) {
    let arr = req.body.arr;
    let count = 0;
    for(let i=0; i<arr.length; i++) {
        count = count+1;
    }
    containerless.respond(count);
});