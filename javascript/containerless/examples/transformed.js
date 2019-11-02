var containerless00 = require('../dist/index');

let cb = containerless00.cb;
let exp = containerless00.exp;
cb.trace.newTrace();
let $test = exp.boolean(false);
cb.trace.traceLet("fun0", exp.clos({}));

function fun0(req) {
  let [$clos, $req] = cb.trace.traceFunctionBody("'ret");
  cb.trace.traceLet("req", $req);
  cb.trace.traceLet("x00", exp.get(exp.get(exp.identifier("req"), "query"), "x"));
  var x00 = req.query.x;
  $test = exp.binop(">", exp.identifier("x00"), exp.number(10));

  if (x00 > 10) {
    cb.trace.traceIfTrue($test);
    cb.trace.traceFunctionCall("app1", [exp.from(exp.identifier("containerless00"), "respond"), exp.string("42")]);
    let app1 = containerless00.respond("42");
    cb.trace.exitBlock();
    cb.trace.traceBreak("'ret", exp.op1("void", exp.number(0)));
    return void 0;
  } else {
    cb.trace.traceIfFalse($test);
  }

  cb.trace.exitBlock();
  cb.trace.traceFunctionCall("app2", [exp.from(exp.identifier("containerless00"), "respond"), exp.string("24")]);
  let app2 = containerless00.respond("24");
  cb.trace.exitBlock();
  cb.trace.exitBlock();
}

cb.trace.traceFunctionCall("app0", [exp.from(exp.identifier("containerless00"), "listen"), exp.identifier("fun0")]);
let app0 = containerless00.listen(fun0);
cb.trace.exitBlock();
cb.trace.exitBlock();
