let containerless = require("../containerless");
let str = 'Got a response!';
containerless.listen(function(req, resp) {
    // console.log(str);
    console.error(str);
    resp(req);
});