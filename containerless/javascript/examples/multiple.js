let containerless = require('containerless');

let foo = 42;

containerless.listen(function(req, resp) {
    console.error('Got a response');
    let bar = foo + 1;
    resp(req);

    containerless.get('http://people.cs.umass.edu/~emilyherbert/', function(response) {
        console.error(response);
        let baz = foo + bar;
    });

    console.error('All done!');
});