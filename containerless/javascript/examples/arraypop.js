let containerless = require('containerless');
containerless.listen(function(req) {
    let x = [0];
    for(let i=0; i<x.length; i++) {
        x.pop();
    }
    containerless.respond("Done!");
});