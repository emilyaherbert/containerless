import * as types from '../ts/types';

test('trivial trace', () => {
    let trace = types.emptyPartialTrace();
    console.log(trace.trace);
    types.traceLet(trace, 'x', types.number(12));
    console.log(trace.trace);
    types.traceLet(trace, 'y', types.number(120));
    console.log(trace.trace);
});

test('condition with retracing', () => {
    let trace = types.emptyPartialTrace();
    types.traceIfTrue(trace, types.number(0));
    types.traceComplete(trace, types.number(100));
    console.log(trace.trace);
    types.traceIfFalse(trace, types.number(0));
    console.log(trace.trace);
    types.traceComplete(trace, types.number(200));
    console.log(trace.trace);
});