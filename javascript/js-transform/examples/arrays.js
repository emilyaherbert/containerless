var decontainerizable = require('../dist/index');

decontainerizable.listen(function(req, resp) {
    let arr = [1,2,3];
    arr[0] = 5;
    console.log(arr[0]);
    resp(req);
});