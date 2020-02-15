let containerless = require('containerless');
containerless.listen(function(req) { 
    console.log('Got a response');
    containerless.respond('Hello, world!\n');
});
