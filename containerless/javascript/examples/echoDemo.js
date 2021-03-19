var decontainerizable = require('containerless');

var cb = decontainerizable.cb;
var exp = decontainerizable.exp;

cb.trace.newTrace();

cb.trace.traceLet('F', exp.clos({ }));
function F(req, resp) {
    // or... t = decontainerizable.getTrace();

    // #2 <-
    let [clos, $req, $resp] = cb.trace.traceFunctionBody('$return');
    cb.trace.traceLet('req', $req);
    cb.trace.traceLet('resp', $resp);

    console.log('Got a response');

    cb.trace.pushArgs([exp.identifier('resp'), exp.identifier('req')]);
    // #3 ->
    resp(req);

    cb.trace.exitBlock();
}

cb.trace.pushArgs([exp.from(exp.identifier('decontainerizble'), 'listen'), exp.identifier('F')]);
// #1 ->
decontainerizable.listen(F);

cb.trace.exitBlock();