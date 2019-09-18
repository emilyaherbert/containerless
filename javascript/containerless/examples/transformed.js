var containerless00 = require('../dist/index');

let cb = containerless00.cb;
let exp = containerless00.exp;
cb.trace.newTrace();
cb.trace.traceLet("fun000", exp.clos({}));

function fun000(req, resp) {
  let [clos, $req, $resp] = cb.trace.traceFunctionBody("'ret");
  cb.trace.traceLet("req", $req);
  cb.trace.traceLet("resp", $resp);
  let $test = exp.binop("===", exp.from(exp.identifier("req"), "path"), exp.string("/login"));

  if (req.path === '/login') {
    cb.trace.traceIfTrue($test);
    cb.trace.traceLet("fun100", exp.clos({}));

    function fun100(response) {
      let [clos, $response] = cb.trace.traceFunctionBody("'ret");
      cb.trace.traceLet("response", $response);
      cb.trace.traceFunctionCall("app200", [exp.from(exp.identifier("containerless00"), "respond"), exp.string("Someone tried to log in.")]);
      var app200 = containerless00.respond("Someone tried to log in.");
      cb.trace.exitBlock();
      cb.trace.exitBlock();
    }

    cb.trace.traceFunctionCall("app100", [exp.from(exp.identifier("containerless00"), "get"), exp.string("https://emilyaherbert.github.io/authorize.txt"), exp.identifier("fun100")]);
    var app100 = containerless00.get("https://emilyaherbert.github.io/authorize.txt", fun100);
    cb.trace.exitBlock();
  } else {
    cb.trace.traceIfFalse($test);
    cb.trace.traceFunctionCall("app300", [exp.from(exp.identifier("containerless00"), "respond"), exp.identifier("req")]);
    var app300 = containerless00.respond(req);
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
