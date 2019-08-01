var decontainerizable = require('../dist/index');

var cb = decontainerizable.cb;
var identifier = decontainerizable.identifier;
var from = decontainerizable.from;
var clos = decontainerizable.clos;

cb.trace.newTrace();

cb.trace.traceLet('F', clos({}));
function F(req, resp) {
    // #2 <-
    let [$clos, $req, $resp] = cb.trace.traceFunctionBody('$return');
    cb.trace.traceLet('req', $req);
    cb.trace.traceLet('resp', $resp);

    console.log('Got a response');

    cb.trace.pushArgs([identifier('resp'), identifier('req')]);
    // #3 ->
    resp(req);

    /*

        This function body prints, and calls resp(req).

        resp(req)

    */

    cb.trace.exitBlock();
}

cb.trace.pushArgs('b', [from(identifier('cb'), 'listen'), identifier('F')])
// #1 ->
cb.listen(F);

cb.trace.exitBlock();