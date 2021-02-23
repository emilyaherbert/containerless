var containerless00 = require('containerless');

let cb = containerless00.cb;
let exp = containerless00.exp;
cb.trace.newTrace();
let $test = exp.boolean(false);
cb.trace.traceLet("fun0", exp.clos({}));

function fun0(req) {
  let [$clos, $req] = cb.trace.traceFunctionBody("'ret");
  cb.trace.traceLet("req", $req);
  cb.trace.tracePrimApp("console.log", [exp.string("Got a response")]);
  let app1 = console.log('Got a response');
  cb.trace.traceFunctionCall("app2", [exp.from(exp.identifier("containerless00"), "helloWithID"), exp.get(exp.get(exp.identifier("req"), "query"), "id")]);
  let app2 = containerless00.helloWithID(req.query.id);
  cb.trace.exitBlock();
  cb.trace.exitBlock();
}

cb.trace.traceFunctionCall("app0", [exp.from(exp.identifier("containerless00"), "listen"), exp.identifier("fun0")]);
let app0 = containerless00.listen(fun0);
cb.trace.exitBlock();
cb.trace.exitBlock();
