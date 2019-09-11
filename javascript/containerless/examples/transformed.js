var containerless00 = require('../dist/index');

let cb = containerless00.cb;
let exp = containerless00.exp;
cb.trace.newTrace();
cb.trace.traceLet("fun000", exp.obj({}));

function fun000(req, resp) {
  let [clos, $req, $resp] = cb.trace.traceFunctionBody("'return");
  cb.trace.traceLet("req", $req);
  cb.trace.traceLet("resp", $resp);
  cb.trace.traceLet("arr00", exp.array([exp.number(1), exp.number(2), exp.number(3)]));
  var arr00 = [1, 2, 3];
  cb.trace.traceSet(exp.index(exp.identifier("arr00"), 0), exp.number(5));
  arr00[0] = 5;
  cb.trace.tracePrimApp("console.log", [exp.index(exp.identifier("arr00"), 0)]);
  var app100 = console.log(arr00[0]);
  cb.trace.traceFunctionCall("app200", [exp.identifier("resp"), exp.identifier("req")]);
  var app200 = resp(req);
  cb.trace.exitBlock();
  cb.trace.exitBlock();
}

cb.trace.traceFunctionCall("app000", [exp.from(exp.identifier("containerless00"), "listen"), exp.identifier("fun000")]);
var app000 = containerless00.listen(fun000);
cb.trace.exitBlock();
cb.trace.exitBlock();
cb.trace.getTrace();
