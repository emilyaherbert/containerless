var containerless00 = require('../dist/index');

let cb = containerless00.cb;
let exp = containerless00.exp;
cb.trace.newTrace();
let $test = exp.boolean(false);
cb.trace.traceLet("fun000", exp.clos({}));

function fun000(req) {
  let [$clos, $req] = cb.trace.traceFunctionBody("'ret");
  cb.trace.traceLet("req", $req);
  cb.trace.traceLet("body00", exp.from(exp.identifier("req"), "body"));
  // TODO(emily): Fix so that you can chain member expressions.
  var body00 = req.body;
  $test = exp.binop("===", exp.from(exp.identifier("req"), "path"), exp.string("/login"));

  if (req.path === '/login') {
    cb.trace.traceIfTrue($test);
    cb.trace.traceLet("fun100", exp.clos({}));

    function fun100(resp) {
      let [$clos, $resp] = cb.trace.traceFunctionBody("'ret");
      cb.trace.traceLet("resp", $resp);
      $test = exp.binop("||", exp.binop("||", exp.binop("||", exp.binop("===", exp.from(exp.identifier("resp"), "username"), exp.undefined_), exp.binop("===", exp.from(exp.identifier("resp"), "password"), exp.undefined_)), exp.binop("===", exp.from(exp.identifier("body00"), "username"), exp.undefined_)), exp.binop("===", exp.from(exp.identifier("body00"), "password"), exp.undefined_));

      if (resp.username === undefined || resp.password === undefined || body00.username === undefined || body00.password === undefined) {
        cb.trace.traceIfTrue($test);
        cb.trace.traceFunctionCall("app200", [exp.from(exp.identifier("containerless00"), "respond"), exp.string("Username and password not found.\n")]);
        var app200 = containerless00.respond("Username and password not found.\n");
        cb.trace.exitBlock();
      } else {
        cb.trace.traceIfFalse($test);
        $test = exp.binop("&&", exp.binop("===", exp.from(exp.identifier("resp"), "username"), exp.from(exp.identifier("body00"), "username")), exp.binop("===", exp.from(exp.identifier("resp"), "password"), exp.from(exp.identifier("body00"), "password")));

        if (resp.username === body00.username && resp.password === body00.password) {
          cb.trace.traceIfTrue($test);
          cb.trace.traceFunctionCall("app300", [exp.from(exp.identifier("containerless00"), "respond"), exp.string("Login successful!\n")]);
          var app300 = containerless00.respond("Login successful!\n");
          cb.trace.exitBlock();
        } else {
          cb.trace.traceIfFalse($test);
          cb.trace.traceFunctionCall("app400", [exp.from(exp.identifier("containerless00"), "respond"), exp.string("Invalid username or password.\n")]);
          var app400 = containerless00.respond("Invalid username or password.\n");
          cb.trace.exitBlock();
        }

        cb.trace.exitBlock();
      }

      cb.trace.exitBlock();
      cb.trace.exitBlock();
    }

    cb.trace.traceFunctionCall("app100", [exp.from(exp.identifier("containerless00"), "get"), exp.string("https://emilyaherbert.github.io/authorize.json"), exp.identifier("fun100")]);
    var app100 = containerless00.get("https://emilyaherbert.github.io/authorize.json", fun100);
    cb.trace.exitBlock();
  } else {
    cb.trace.traceIfFalse($test);
    cb.trace.traceFunctionCall("app500", [exp.from(exp.identifier("containerless00"), "respond"), exp.string("Unknown command.\n")]);
    var app500 = containerless00.respond("Unknown command.\n");
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
