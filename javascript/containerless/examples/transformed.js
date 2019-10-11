var containerless00 = require('../dist/index');

let fun0 = function funExpr0(req) {
  let app1 = containerless00.respond('Hello, world');
};

let app0 = containerless00.listen(fun0);
var containerless00 = require('../dist/index');

let cb = containerless00.cb;
let exp = containerless00.exp;
cb.trace.newTrace();
let $test = exp.boolean(false);
cb.trace.traceLet("fun000", exp.clos({}));

function fun000(req) {
  let [$clos, $req] = cb.trace.traceFunctionBody("'ret");
  cb.trace.traceLet("req", $req);
  cb.trace.traceFunctionCall("app100", [exp.from(exp.identifier("containerless00"), "respond"), exp.string("Hello, world")]);
  var app100 = containerless00.respond('Hello, world');
  cb.trace.exitBlock();
  cb.trace.exitBlock();
}

cb.trace.traceFunctionCall("app000", [exp.from(exp.identifier("containerless00"), "listen"), exp.identifier("fun000")]);
var app000 = containerless00.listen(fun000);
cb.trace.exitBlock();
cb.trace.exitBlock();
cb.trace.getTrace();
