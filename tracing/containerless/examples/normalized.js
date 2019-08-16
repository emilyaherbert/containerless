var decontainerizable = require('../dist/index');

let fun0 = function funExpr0(req, resp) {
  let app1 = console.log('Got a response');
  let app2 = resp(req);
};

let app0 = decontainerizable.listen(fun0);
