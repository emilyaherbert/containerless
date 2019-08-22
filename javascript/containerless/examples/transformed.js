var containerless00 = require('../dist/index');

let cb = containerless00.cb;
let exp = containerless00.exp;
cb.trace.newTrace();
cb.trace.traceLet("fun000", exp.obj({}));

function fun000(req, resp) {
  let [clos, $req, $resp] = cb.trace.traceFunctionBody("'return");
  cb.trace.traceLet("req", $req);
  cb.trace.traceLet("resp", $resp);
  let $test = exp.binop("===", exp.from(exp.identifier("req"), "path"), exp.string("/ping"));

  if (req.path === '/ping') {
    cb.trace.traceIfTrue($test);
    cb.trace.traceFunctionCall("app100", [exp.identifier("resp"), exp.obj({
      path: exp.string("pong")
    })]);
    var app100 = resp({
      path: 'pong'
    });
    cb.trace.exitBlock();
  } else {
    cb.trace.traceIfFalse($test);
    cb.trace.traceFunctionCall("app200", [exp.identifier("resp"), exp.obj({
      path: exp.string("ping")
    })]);
    var app200 = resp({
      path: 'ping'
    });
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
