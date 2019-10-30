let containerless = require('containerless');

containerless.listen(function(req) {
    if(req.path === '/upload') {
        if(req.body !== undefined) {
            containerless.post({
		url: 'http://10.200.0.1:7999/upload',
                body: req.body
            }, function(resp) {
	        if(resp.error !== undefined) {
                    containerless.respond("error\n");
                } else {
                    containerless.respond('Uploaded\n');
                }
            });
        } else {
            containerless.respond("No file to upload.\n");
        }
    } else {
        containerless.respond(req.path);
    }
});
