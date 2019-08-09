let containerless = require('../dist/index');

containerless.listen(function(req, resp) {
    console.log('Got a response');
    resp(req);

    containerless.get('http://google.com', function(response) {
        console.log(response);
    })

    console.log("All done!");
});