let containerless = require('containerless');

function upload(baseurl, name, file) {
    let options = {
        url: baseurl + '/' + name + '',
        body: {}
    };
    // Create a doc in the database
    containerless.put(options, function(resp) {
        if(resp === undefined) {
            containerless.respond("No response.");
        } else if(resp.error !== undefined) {
            containerless.respond("Got back an error: " + resp.error);
        } else if(resp.rev === undefined) {
            containerless.respond("Unexpected response.");
        } else {
            let options = {
                url: baseurl + '/' + name + '/' + name + '.json?rev=' + resp.rev,
                body: file
            };
            // Attach a file to the doc we just created
            containerless.put(options, function(resp) {
                if(resp === undefined) {
                    containerless.respond("No response.");
                } else if(resp.error !== undefined) {
                    containerless.respond("Got back an error: " + resp.error);
                } else if(resp.ok === undefined) {
                    containerless.respond("Unexpected response.");
                } else if(resp.ok) {
                    containerless.respond("Success!");
                } else {
                    containerless.respond("Failure :(");
                }
            });
        }
    });
}

function list(baseurl) {
    let uri = baseurl + '/_all_docs';
    containerless.get(uri, function(resp) {
        if(resp === undefined) {
            containerless.respond("No response.");
        } else if(resp.error !== undefined) {
            containerless.respond("Got an error: " + resp.error);
        } else {
            containerless.respond(resp);
        }
    });
}

function get(baseurl, filename) {
    let uri = baseurl + '/' + filename + '/' + filename + '.json';
    containerless.get(uri, function(resp) {
        if(resp === undefined) {
            containerless.respond("No response.");
        } else if(resp.error !== undefined) {
            containerless.respond("Got an error: " + resp.error);
        } else {
            containerless.respond(resp);
        }
    });
}

containerless.listen(function(req) {
    let username = "admin";
    let password = "Wnw4JP0nvgHcdXpiVPxd";
    let ip = "10.200.0.28";
    let database = "myfiles";
    let baseurl = 'http://' + username + ':' + password + '@' + ip + ':30984/' + database;

    if(req === undefined) {
        containerless.respond(req);
    } else {
        if(req.path === "/list") {
            list(baseurl);
        } else if(req.path === "/get") {
            if(req.query === undefined || req.query.filename === undefined) {
                containerless.respond("Malformed request.");
            } else {
                get(baseurl, req.query.filename);
            }  
        } else if(req.path === "/upload") {
            if(req.query === undefined || req.query.filename === undefined || req.body === undefined) {
                containerless.respond("Malformed request.");
            } else {
                upload(baseurl, req.query.filename, req.body);
            }
        } else {
            containerless.respond("Unknown command.");
        }
    }
});