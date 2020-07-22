let containerless = require("../dist/index");

containerless.listen(function(req) {
    let arr = req.body.arr;
    for(let i=0; i<arr.length; i++) {
        console.log(arr[i]);
    }
    containerless.respond("bye");
});