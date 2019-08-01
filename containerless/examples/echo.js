var decontainerizable = require('../dist/index');

var cb = decontainerizable.cb;
var identifier = decontainerizable.identifier;
var number = decontainerizable.number;
var from = decontainerizable.from;
var clos = decontainerizable.clos;

cb.trace.newTrace();

cb.trace.traceLet('foo', number(5));
let foo = 5;

cb.trace.traceLet('F', clos({ 'foo': identifier('foo') }));
function F(req, resp) {
    // #2 <-
    let [$clos, $req, $resp] = cb.trace.traceFunctionBody('$return');
    cb.trace.traceLet('req', $req);
    cb.trace.traceLet('resp', $resp);

    console.log('Got a response');

    cb.trace.traceLet('bar', from($clos, 'foo'));
    let bar = foo;

    cb.trace.pushArgs([identifier('resp'), identifier('req')]);
    // #3 ->
    resp(req);

    cb.trace.exitBlock();
}

cb.trace.pushArgs([from(identifier('cb'), 'listen'), identifier('F')]);
// #1 ->
cb.listen(F);

cb.trace.exitBlock();