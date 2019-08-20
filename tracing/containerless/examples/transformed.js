var containerless00 = require('../dist/index');

let cb = containerless00.cb;
let exp = containerless00.exp;
cb.trace.newTrace();
cb.trace.traceLet("foo00", exp.number(42));
var foo00 = 42;
cb.trace.traceLet("fun000", exp.clos({
  foo00: exp.identifier("foo00")
}));

function fun000(req, resp) {
  let [clos, $req, $resp] = cb.trace.traceFunctionBody("$return");
  cb.trace.traceLet("req", $req);
  cb.trace.traceLet("resp", $resp);
  cb.trace.tracePrimApp("console.log", [exp.from(exp.identifier("console"), "error"), exp.string("Got a response")]);
  var app100 = console.error('Got a response');
  cb.trace.traceLet("bar00", exp.binop("+", exp.from(clos, "foo00"), exp.number(1)));
  var bar00 = foo00 + 1;
  cb.trace.traceFunctionCall("app200", [exp.identifier("resp"), exp.identifier("req")]);
  var app200 = resp(req);
  cb.trace.exitBlock();
  cb.trace.traceLet("fun100", exp.clos({
    foo00: exp.from(clos, "foo00"),
    bar00: exp.identifier("bar00")
  }));

  function fun100(response) {
    let [clos, $response] = cb.trace.traceFunctionBody("$return");
    cb.trace.traceLet("response", $response);
    cb.trace.tracePrimApp("console.log", [exp.from(exp.identifier("console"), "error"), exp.identifier("response")]);
    var app500 = console.error(response);
    cb.trace.traceLet("baz00", exp.binop("+", exp.from(clos, "foo00"), exp.from(clos, "bar00")));
    var baz00 = foo00 + bar00;
    cb.trace.exitBlock();
  }

  cb.trace.traceFunctionCall("app300", [exp.from(exp.identifier("containerless00"), "get"), exp.string("http://people.cs.umass.edu/~emilyherbert/"), exp.identifier("fun100")]);
  var app300 = containerless00.get('http://people.cs.umass.edu/~emilyherbert/', fun100);
  cb.trace.exitBlock();
  cb.trace.tracePrimApp("console.log", [exp.from(exp.identifier("console"), "error"), exp.string("All done!")]);
  var app400 = console.error('All done!');
  cb.trace.exitBlock();
}

cb.trace.traceFunctionCall("app000", [exp.from(exp.identifier("containerless00"), "listen"), exp.identifier("fun000")]);
var app000 = containerless00.listen(fun000);
cb.trace.exitBlock();
cb.trace.exitBlock();
cb.trace.getTrace();
