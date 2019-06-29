var decontainerizable = require('../dist/index');

decontainerizable.listen(function(req, resp) {
    console.log('Got a response');
    resp(req);
});