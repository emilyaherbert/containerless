let containerless = require('../dist/index');

containerless.listen(function(req, resp) {
    console.log(req);
    console.log(resp);
    if(req.path === "/rust") {
        let retStr = "Yay!";
        console.log(retStr);
        resp({ path: retStr });
    }
    let retStr = ":(";
    console.log(retStr);
    resp({ path: retStr });
});