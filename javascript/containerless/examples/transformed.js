var containerless00 = require("../dist/index");

let cb = containerless00.cb;
let exp = containerless00.exp;
cb.trace.newTrace();
cb.trace.traceLet("fun000", exp.obj({}));

function fun000(req, resp) {
  let [clos, $req, $resp] = cb.trace.traceFunctionBody("'return");
  cb.trace.traceLet("req", $req);
  cb.trace.traceLet("resp", $resp);
  cb.trace.traceLet("ret00", exp.obj({
    path: exp.string("pizza")
  }));
  var ret00 = {
    path: 'pizza'
  };
  let $test = exp.binop("===", exp.from(exp.identifier("req"), "path"), exp.string("/ping"));

  if (req.path === '/ping') {
    cb.trace.traceIfTrue($test);
    cb.trace.traceSet(exp.from(exp.identifier("ret00"), "path"), exp.string("pong"));
    ret00.path = 'pong';
    cb.trace.traceFunctionCall("app100", [exp.identifier("resp"), exp.identifier("ret00")]);
    var app100 = resp(ret00);
    cb.trace.exitBlock();
  } else {
    cb.trace.traceIfFalse($test);
    cb.trace.traceSet(exp.from(exp.identifier("ret00"), "path"), exp.string("ping"));
    ret00.path = 'ping';
    cb.trace.traceFunctionCall("app200", [exp.identifier("resp"), exp.identifier("ret00")]);
    var app200 = resp(ret00);
    cb.trace.exitBlock();
  }

  cb.trace.exitBlock();
  cb.trace.exitBlock();
}

cb.trace.traceFunctionCall("app000", [exp.from(exp.identifier("containerless00"), "listen"), exp.identifier("fun000")]);
var app000 = containerless00.listen(fun000);
cb.trace.exitBlock();
cb.trace.exitBlock();
cb.trace.getTrace();
