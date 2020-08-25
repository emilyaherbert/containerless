let containerless = require('containerless');

function upload(baseurl, name, file) {
    // Create a doc in the database
    let uri = baseurl + '/' + name + '"';
    containerless.put(uri, function(resp) {
        if(resp === undefined) {
            containerless.respond("No response.");
        } else if(resp._rev === undefined) {
            containerless.respond("Unexpected response.");
        } else {

            // Attach a file to the doc we just created
            let options = {
                url: baseurl + '/' + name + '/' + name + '.json?rev=' + resp._rev,
                body: file
            };
            containerless.put(options, function(resp) {
                if(resp === undefined) {
                    containerless.respond("No response.");
                } else if(resp.error !== undefined) {
                    containerless.respond("Error!");
                } else {
                    containerless.respond(resp);
                }
            })
        }
    });
}

function list(baseurl) {
    let uri = baseurl + '/_all_docs"';
    containerless.get(uri, function(resp) {
        if(resp === undefined) {
            containerless.respond("No response.");
        } else if(resp.error !== undefined) {
            containerless.respond("Error!");
        } else {
            containerless.respond(resp);
        }
    });
}

containerless.listen(function(req) {
    if(req === undefined || req.query === undefined || req.query.username === undefined || req.query.password === undefined) {
        containerless.respond("Malformed request.");
    } else {
        let baseurl = 'http://' + req.query.username + ':' + req.query.password + '@10.200.0.28:30984/myfiles"';
        let filename = req.query.filename;

        if(req.path === "/list") {
            list(baseurl);
        } else if(req.path === "/upload") {
            if(req.body === undefined) {
                containerless.respond("No file to upload!");
            } else {
                upload(baseurl, filename, req.body);
            }           
        } else {
            containerless.respond("Unknown command.");
        }
    }
});