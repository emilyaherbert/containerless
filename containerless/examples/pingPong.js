var decontainerizable = require('../dist/index');

var cb = decontainerizable.cb;
var identifier = decontainerizable.identifier;
var number = decontainerizable.number;
var from = decontainerizable.from;
var clos = decontainerizable.clos;
var binop = decontainerizable.binop;
var string = decontainerizable.string;

cb.trace.newTrace();

cb.trace.traceLet('G', clos({ }));
function G(req, resp) {
    let [$clos, $req, $resp] = cb.trace.traceFunctionBody('$return');
    cb.trace.traceLet('req', $req);
    cb.trace.traceLet('resp', $resp);

    let $test = binop('===', identifier('req'), string('ping'));
    if(req.path === '/ping') {
        cb.trace.traceIfTrue($test);
        cb.trace.pushArgs([identifier('resp'), string('{ path: "pong" }')]);
        resp({ path: 'pong' });
    } else {
        cb.trace.traceIfFalse($test);
        cb.trace.pushArgs([identifier('resp'), string('{ path: "ping" }')]);
        resp({ path: 'ping' });
    }
    cb.trace.exitBlock();

    cb.trace.exitBlock();
}

cb.trace.pushArgs([from(identifier('cb'), 'listen'), identifier('G')]);
cb.listen(G);

cb.trace.exitBlock();