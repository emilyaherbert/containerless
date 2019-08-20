import {
    while_, break_, label, block, let_, set, number, if_, callback,
    identifier, string, binop, unknown, undefined_, clos, from, froms
} from '../ts/exp';
import {
    newTrace
} from '../ts/tracing'
import { Callbacks } from '../ts/callbacks';

test('trivial, hand-constructed trace', () => {
    let t = newTrace();
    t.traceLet('x', number(12));
    t.traceLet('y', number(120));
    t.exitBlock();
    expect(t.getTrace()).toMatchObject(
        block([
            let_('x', number(12)),
            let_('y', number(120))]));
});

test('trivial, hand-constructed re-tracing', () => {
    let t = newTrace();
    t.traceLet('x', number(12));
    t.traceLet('y', number(120));
    t.exitBlock();
    // pretend we ran the program again
    t.newTrace();
    t.traceLet('x', number(12));
    t.traceLet('y', number(120));
    t.exitBlock();
    expect(t.getTrace()).toMatchObject(
        block([
            let_('x', number(12)),
            let_('y', number(120))]));
});

test('re-tracing: both sides of a conditional', () => {
    let t = newTrace();
    t.traceIfTrue(number(0));
    t.traceLet('x', number(100));
    t.exitBlock();
    t.traceLet('z', number(300));
    t.exitBlock();
    // pretend we ran the program again
    t.newTrace();
    t.traceIfFalse(number(0));
    t.traceLet('y', number(200));
    t.exitBlock();
    t.traceLet('z', number(300));
    t.exitBlock();

    expect(t.getTrace()).toMatchObject(
        block([
            if_(number(0),
                [let_('x', number(100))],
                [let_('y', number(200))]),
            let_('z', number(300))]));
});

test('callback in trace', () => {
    let t = newTrace();
    t.traceLet('x', number(10));
    let innerTrace = t.traceCallback('dummy-event', identifier('x'), ['x'], clos({}));
    t.traceLet('y', number(20));
    t.exitBlock();

    // Now, the callback runs
    innerTrace.traceLet('z', number(30));
    innerTrace.exitBlock();

    expect(t.getTrace()).toMatchObject(
        block([
            let_('x', number(10)),
            callback('dummy-event', identifier('x'), ['x'], clos({}), [
                let_('z', number(30))]),
            let_('y', number(20))]));
});

test('re-tracing: apply function twice with different control flow', () => {
    let t = newTrace();

    t.traceLet('F', clos({}));
    function F(x: any) {
        let [clos, $x] = t.traceFunctionBody('$return');
        t.traceLet('x', $x);

        t.traceLet('ret', number(0));
        let ret = 0;
        let $cond = binop('>', identifier('x'), number(10));
        if(x > 10) {
            t.traceIfTrue($cond);
            t.traceSet(identifier('ret'), number(42));
            ret = 42;
        } else {
            t.traceIfFalse($cond);
            t.traceSet(identifier('ret'), number(24));
            ret = 24;
        }
        t.exitBlock();

        t.traceBreak('$return', identifier('ret'));
        return ret;

        t.exitBlock();
    }

    t.traceFunctionCall('w', [identifier('F'), number(11)]);
    let w = F(11);
    t.exitBlock();

    t.traceFunctionCall('v', [identifier('F'), number(9)]);
    let v = F(9);
    t.exitBlock();

    t.exitBlock();

    expect(t.getTrace()).toMatchObject(block([
        let_('F', clos({ })),
        let_('w', block([
            label('$return', [
                let_('x', number(11)),
                let_('ret', number(0)),
                if_(binop('>', identifier('x'), number(10)),
                    [ set(identifier('ret'), number(42)) ],
                    [ unknown() ]),
                break_('$return', identifier('ret'))
            ])])),
        let_('v', block([
            label('$return', [
                let_('x', number(9)),
                let_('ret', number(0)),
                    if_(binop('>', identifier('x'), number(10)),
                        [ unknown() ],
                        [ set(identifier('ret'), number(24)) ]),
                    break_('$return', identifier('ret'))])]))]));

});

test('exit fun from within if', () => {
    let t = newTrace();

    t.traceLet('F', clos({ }));
    function F(x: any) {
        let [clos, $x] = t.traceFunctionBody('$return');
        t.traceLet('x', $x);

        let $cond = binop('>', identifier('x'), number(10));
        if(x > 10) {
            t.traceIfTrue($cond);
            t.traceBreak('$return', number(42));
            return 42;
        } else {
            t.traceIfFalse($cond);
            t.traceBreak('$return', number(24));
            return 24;
        }
        t.exitBlock();

        t.exitBlock();
    }

    t.traceFunctionCall('w', [identifier('F'), number(11)]);
    let w = F(11);
    t.exitBlock();

    t.traceFunctionCall('v', [identifier('F'), number(9)]);
    let v = F(9);
    t.exitBlock();

    t.exitBlock();

    expect(t.getTrace()).toMatchObject(block([
        let_('F', clos({ })),
        let_('w', block([
            label('$return', [
                let_('x', number(11)),
                if_(binop('>', identifier('x'), number(10)),
                    [ break_('$return', number(42)) ],
                    [ unknown() ]),
                unknown() ])])),
        let_('v', block([
            label('$return', [
                let_('x', number(9)),
                if_(binop('>', identifier('x'), number(10)),
                    [ unknown() ],
                    [ break_('$return', number(24)) ]),
                    unknown() ])]))]));
});

test('tracing with callback library', (done) => {
    let cb = new Callbacks();
    cb.trace.pushArgs([clos({}), string('hello'), clos({})]);
    cb.immediate('hello', (str) => {
        let [clos, $str] = cb.trace.traceFunctionBody('$return');
        cb.trace.traceLet('str', $str);

        cb.trace.traceLet('x', number(100));
        let x = 100;
        cb.trace.traceLet('z', identifier('str'));
        let z = str;
        
        cb.trace.exitBlock();

        setImmediate(() => {
            expect(cb.trace.getTrace()).toMatchObject(
                block([
                    callback('immediate', string('hello'), ['clos', '$x'], clos({}), [
                        label('$return', [
                            let_('str', identifier('$x')),
                            let_('x', number(100)),
                            let_('z', identifier('str'))])
                        ]),
                    let_('y', number(200))]));
            done();
        });
    });
    cb.trace.traceLet('y', number(200));
    cb.trace.exitBlock();

});

test('callback that receives multiple events', () => {
    let cb = new Callbacks();

    let captured = 42;
    cb.trace.traceLet('captured', number(42));
    cb.trace.traceLet('F', clos({ captured: identifier('captured') }));
    function F(value: any) {

        let [clos, $value] = cb.trace.traceFunctionBody('$return');
        cb.trace.traceLet('value', $value);
        cb.trace.traceLet('ret', from(clos, 'captured'));
        let ret = captured;

        let $cond = binop('>', identifier('value'), number(0));
        if (value > 0) {
            cb.trace.traceIfTrue($cond);
            cb.trace.traceSet(identifier('ret'), number(200));
            ret = 200;
        }
        else {
            cb.trace.traceIfFalse($cond);
            cb.trace.traceSet(identifier('ret'), number(-200));
            ret = -200;
        }
        cb.trace.exitBlock(); // exitBlock() for if statement.

        cb.trace.exitBlock(); // exitBlock() for label '$return'.
    }

    cb.trace.traceFunctionCall('sender', [from(identifier('cb'), 'mockCallback'), identifier('F'), number(200)]);
    let sender = cb.mockCallback(F, 200);
    cb.trace.exitBlock();
    cb.trace.exitBlock(); // end turn

    sender(-100);
    sender(100);

    expect(cb.trace.getTrace()).toMatchObject(
        block([
            let_('captured', number(42)),
            let_('F', clos({ captured: identifier('captured') })),
            let_('sender', block([
                callback('mock', number(200), ['clos', '$response'], identifier('F'), [
                    label('$return', [
                        let_('value', identifier('$response')),
                        let_('ret', from(identifier('clos'), 'captured')),
                        if_(binop('>', identifier('value'), number(0)), [
                            set(identifier('ret'), number(200))], [
                            set(identifier('ret'), number(-200))])])])]))]));
});

test('while loop', () => {
    let t = newTrace();

    t.traceLet("x", number(2));
    let x = 2;
    t.traceLet("y", number(0));
    let y = 0;

    t.traceWhile(binop(">", identifier("x"), number(0)));
    while(x > 0) {
        t.traceLoop();

        if(x > 1) {
            t.traceIfTrue(binop('>', identifier('x'), number(1)));
            t.traceSet(identifier('y'), binop('+', identifier('y'), number(2)));
            y = y + 2;
        } else {
            t.traceIfFalse(binop('>', identifier('x'), number(1)));
            t.traceSet(identifier('y'), binop('+', identifier('y'), number(3)));
            y = y + 3;
        }
        t.exitBlock();

        t.traceSet(identifier('x'), binop("-", identifier("x"), number(1)));
        x = x - 1;
    };
    t.exitBlock();
    t.exitBlock();

    expect(t.getTrace()).toMatchObject(block([
        let_('x', number(2)),
        let_('y', number(0)),
        while_(binop('>', identifier('x'), number(0)), [
            if_(binop('>', identifier('x'), number(1)),
                [ set(identifier('y'), binop('+', identifier('y'), number(2))) ],
                [ set(identifier('y'), binop('+', identifier('y'), number(3))) ]),
            set(identifier('x'), binop('-', identifier('x'), number(1)))
        ])]));
});

test('label with an immediate break', () => {
    let t = newTrace();

    t.traceLabel('things');
    things : {
        t.traceLet('x', number(77));
        let x = 77;
        t.traceBreak('things', undefined_);
        break things;
        // NOTE(arjun): The code below will never run, so it is almost pointless
        // that we have written it out by hand. But, this is the kind of code
        // the compiler will generate.
        t.traceLet('oops', number(666));
        let oops = 666;
        t.exitBlock();
    }

    t.traceLet('keyboard', number(11));
    let keyboard = 11;

    t.exitBlock();

    expect(t.getTrace()).toMatchObject(block([
        label('things', [
            let_('x', number(77)),
            break_('things', undefined_)]),
        let_('keyboard', number(11))]));
});

test('label without a break', () => {
    let t = newTrace();

    t.traceLabel('things');
    things: {
        t.traceLet('x', number(77));
        let x = 77;
        t.traceLet('oops', number(666));
        let oops = 666;
        t.exitBlock();
    }

    t.traceLet('keyboard', number(11));
    let keyboard = 11;

    t.exitBlock();

    expect(t.getTrace()).toMatchObject(block([
        label('things', [
            let_('x', number(77)),
            let_('oops', number(666))]),
        let_('keyboard', number(11))]));
});

test('nested labels', () => {
    let t = newTrace();

    t.traceLabel('things');
    things: {
        t.traceLet('x', number(1));
        let x = 1;

        t.traceLabel('betwixt');
        betwixt: {
            t.traceLet('y', number(2));
            let y = 2;

            t.traceLabel('stuff');
            stuff: {
                t.traceLet('z', number(3));
                let z = 3;
                t.traceBreak('betwixt', undefined_);
                break betwixt;
                t.exitBlock();
            }

            // NOTE(arjun): Dead code, but the kind that we will generate
            t.traceLet('there', number(50));
            let there = 50;
            t.exitBlock();
        }

        t.traceLet('here', number(51));
        let here = 51;
        t.exitBlock();
    }
    t.traceLet('after', number(80));
    let after = 80;

    t.exitBlock();

    expect(t.getTrace()).toMatchObject(block([
        label('things', [
            let_('x', number(1)),
            label('betwixt', [
                let_('y', number(2)),
                label('stuff', [
                    let_('z', number(3)),
                    break_('betwixt', undefined_)]),
                unknown()]),
            let_('here', number(51))]),
        let_('after', number(80))]));
});

test('label and if and break', () => {
    let t = newTrace();

    t.traceLabel('things');
    things: {
        t.traceLet('x', number(77));
        let x = 77;
        if(x > 10) {
            t.traceIfTrue(binop('>', identifier('x'), number(10)));
            t.traceLet('y', number(333));
            let y = 333;
            t.traceBreak('things', undefined_);
            break things;
        } else {
            t.traceIfFalse(binop('>', identifier('x'), number(10)));
            t.traceLet('y', number(444));
            let y = 444;
            t.traceBreak('things', undefined_);
            break things;
        }
        t.exitBlock();
        t.traceLet('oops', number(666));
        let oops = 666;
        t.exitBlock();
    }

    t.traceLet('keyboard', number(11));
    let keyboard = 11;
    t.exitBlock();

    t.newTrace(); // new run
    t.traceLabel('things');
    things: {
        // NOTE(arjun): To avoid entangling this test case with function calls,
        // this is a bit of a fudge where we trace let x = 77, but run let x = 8
        // to exercise the other branch.
        t.traceLet('x', number(77));
        let x = 8;
        if(x > 10) {
            t.traceIfTrue(binop('>', identifier('x'), number(10)));
            t.traceLet('y', number(333));
            let y = 333;
            t.traceBreak('things', undefined_);
            break things;
        } else {
            t.traceIfFalse(binop('>', identifier('x'), number(10)));
            t.traceLet('y', number(444));
            let y = 444;
            t.traceBreak('things', undefined_);
            break things;
        }
        t.exitBlock();
        t.traceLet('oops', number(666));
        let oops = 666;
        t.exitBlock();
    }
    t.traceLet('keyboard', number(11));
    t.exitBlock();

    expect(t.getTrace()).toMatchObject(block([
        label('things', [
            let_('x', number(77)),
            if_(binop('>', identifier('x'), number(10)),
                [ let_('y', number(333)),
                  break_('things', undefined_) ],
                [ let_('y', number(444)),
                  break_('things', undefined_) ]),
              unknown()]),
        let_('keyboard', number(11))]));
});

test('if no else', () => {
    /*

        All if's need to have an else.

        if (cond) {
            // body
        }
            ->
        if (cond) {
            t.traceIfTrue(cond);
            // body
        } else {
            t.traceIfFalse(cond);
        }

    */

    let t = newTrace();

    t.traceLet('x', number(0));
    let x = 0;
    t.traceLet('y', number(0));
    let y = 0;
    if(x > 1) {
        t.traceIfTrue(binop('>', identifier('x'), number(1)));
        t.traceSet(identifier('y'), number(10));
        y = 10;
    } else {
        t.traceIfFalse(binop('>', identifier('x'), number(1)));
    }
    t.exitBlock();
    t.traceLet('z', number(5));
    let z = 5;

    t.newTrace();
    t.traceLet('x', number(0)); // same for testing
    x = 2;
    t.traceLet('y', number(0));
    if(x > 1) {
        t.traceIfTrue(binop('>', identifier('x'), number(1)));
        t.traceSet(identifier('y'), number(10));
        y = 10;
    } else {
        t.traceIfFalse(binop('>', identifier('x'), number(1)));
    }
    t.exitBlock();
    t.traceLet('z', number(5));

    t.exitBlock();

    expect(t.getTrace()).toMatchObject(block([
        let_('x', number(0)),
        let_('y', number(0)),
        if_(binop('>', identifier('x'), number(1)),
            [ set(identifier('y'), number(10)) ],
            [ ]),
        let_('z', number(5))]));
});

test('sometimes break', () => {
    let t = newTrace();

    t.traceLet('x', number(1));
    let x = 1;
    t.traceLabel('myLabel');
    myLabel: {
        if (x > 0) {
            t.traceIfTrue(binop('>', identifier('x'), number(0)));
            t.traceBreak('myLabel', undefined_);
            break myLabel;
        } else {
            t.traceIfFalse(binop('>', identifier('x'), number(0)));
        }
        t.exitBlock();
        t.traceSet(identifier('x'), number(200));
        x = 200;
        t.exitBlock();
    }

    t.newTrace();
    // NOTE(arjun): testing fudge, similar to those above, where we trace x = 1
    // but execute x = 0.
    t.traceLet('x', number(1));
    x = 0;
    t.traceLabel('myLabel');
    myLabel: {
        if (x > 0) {
            t.traceIfTrue(binop('>', identifier('x'), number(0)));
            t.traceBreak('myLabel', undefined_);
            break myLabel;
        } else {
            t.traceIfFalse(binop('>', identifier('x'), number(0)));
        }
        t.exitBlock();
        t.traceSet(identifier('x'), number(200));
        x = 200;
        t.exitBlock();
    }
    t.exitBlock();

    expect(t.getTrace()).toMatchObject(block([
        let_('x', number(1)),
        label('myLabel', [
            if_(binop('>', identifier('x'), number(0)),
                [ break_('myLabel', undefined_) ],
                [ ]),
            set(identifier('x'), number(200))])]));
});


 test('canonical makeAdder higher-order function', () => {
    let t = newTrace();

    t.traceLet('makeAdder', clos({ }));
    function makeAdder(a: any) {
        let [clos, $a] = t.traceFunctionBody('$return')
        t.traceLet('a', $a);

        t.traceLet('add', clos({ 'a':  identifier('a') }));
        function add(b: any) {
            let [clos, $b] = t.traceFunctionBody('$return')
            t.traceLet('b', $b);

            t.traceBreak('$return', binop('+', from(clos, 'a'), identifier('b')));
            return a + b;
            t.exitBlock(); // NOTE(arjun): dead
        };

        t.traceBreak('$return', identifier('add'));
        return add;
        t.exitBlock(); // NOTE(arjun): dead

    };

    t.traceFunctionCall('F', [identifier('makeAdder'), number(9)]);
    let F = makeAdder(9);
    t.exitBlock();

    t.traceFunctionCall('res1', [identifier('F'), number(5)]);
    let res1 = F(5);
    t.exitBlock();

    t.exitBlock(); // end the turn

    expect(t.getTrace()).toMatchObject(block([
        let_('makeAdder', clos({ })),
        let_('F', block([
            label('$return', [
                let_('a', number(9)),
                let_('add', clos({ 'a': identifier('a') })),
                break_('$return', identifier('add'))])])),
        let_('res1', block([
            label('$return', [
                let_('b', number(5)),
                break_('$return', binop('+', from(identifier('F'), 'a'), identifier('b')))
            ])]))]));
 });

 test('function with no return', () => {
    let t = newTrace();

    t.traceLet('foo', number(0));
    let foo = 0;

    t.traceLet('F', clos({ 'foo': identifier('foo') } as any));
    function F(x: any) {
        let [clos, $x] = t.traceFunctionBody('$return');
        t.traceLet('x', $x);

        let $cond = binop('>', identifier('x'), number(10));
        if(x > 10) {
            t.traceIfTrue($cond);
            t.traceSet(from(clos, 'foo'), number(42));
            foo = 42;
        } else {
            t.traceIfFalse($cond);
            t.traceSet(from(clos, 'foo'), number(24));
            foo = 24;
        }
        t.exitBlock();


        t.exitBlock();
    }

    t.traceFunctionCall('w', [identifier('F'), number(11)]);
    let w = F(11);
    t.exitBlock();

    t.traceFunctionCall('v', [identifier('F'), number(9)]);
    let v = F(9);
    t.exitBlock();

    t.exitBlock();

    expect(t.getTrace()).toMatchObject(block([
        let_('foo', number(0)),
        let_('F', clos({ 'foo': identifier('foo') } as any)),
        let_('w', block([
            label('$return', [
                let_('x', number(11)),
                if_(binop('>', identifier('x'), number(10)),
                    [ set(from(identifier('F'), 'foo'), number(42)) ],
                    [ unknown() ])
            ])
        ])),
        let_('v', block([
            label('$return', [
                let_('x', number(9)),
                if_(binop('>', identifier('x'), number(10)),
                    [ unknown() ],
                    [ set(from(identifier('F'), 'foo'), number(24)) ])
            ])
        ])),
    ]));
});

 test('crazy closures', () => {
    let t = newTrace();

    t.traceLet('zero', clos({ } as any));
    function zero() {
        let [clos] = t.traceFunctionBody('$return')

        t.traceLet('foo', number(0));
        let foo = 0;
        
        t.traceLet('one', clos({ 'foo': identifier('foo') }));
        function one(b: any) {
            let [clos, $b] = t.traceFunctionBody('$return')
            t.traceLet('b', $b);

            t.traceSet(from(clos, 'foo'), binop('+', from(clos, 'foo'), identifier('b')));
            foo = foo + b;
        
            t.traceLet('two', clos({ 'foo': from(clos, 'foo') } as any));
            function two(c: any) {
                let [clos, $c] = t.traceFunctionBody('$return')
                t.traceLet('c', $c);

                t.traceSet(from(clos, 'foo'), binop('-', from(clos, 'foo'), identifier('c')));
                foo = foo - c;
            
                t.traceLet('three', clos({ 'foo': from(clos, 'foo') } as any));
                function three() {
                    let [clos] = t.traceFunctionBody('$return')

                    t.traceBreak('$return', from(clos, 'foo'));
                    return foo;
                    t.exitBlock();
                }

                t.traceBreak('$return', identifier('three'));
                return three;
                t.exitBlock();
            }
        
            t.traceBreak('$return', identifier('two'));
            return two;
            t.exitBlock();
        }
      
        t.traceBreak('$return', identifier('one'));
        return one;
        t.exitBlock();
    };
    
    t.traceFunctionCall('add', [identifier('zero')]);
    let add = zero();
    t.exitBlock();

    t.traceFunctionCall('sub', [identifier('add'), number(15)]);
    let sub = add(15);
    t.exitBlock();

    t.traceFunctionCall('toss', [identifier('add'), number(1)]);
    let toss = add(1);
    t.exitBlock();

    t.traceFunctionCall('ret', [identifier('sub'), number(4)]);
    let ret = sub(4);
    t.exitBlock();

    t.traceFunctionCall('foo', [identifier('ret')]);
    let foo = ret();
    t.exitBlock();

    t.exitBlock(); // end the turn

    expect(t.getTrace()).toMatchObject(block([
        let_('zero', clos({ })),
        let_('add', block([
            label('$return', [
                let_('foo', number(0)),
                let_('one', clos({ 'foo': identifier('foo') } as any)),
                break_('$return', identifier('one'))
            ])
        ])),
        let_('sub', block([
            label('$return', [
                let_('b', number(15)),
                set(from(identifier('add'), 'foo'), binop('+', from(identifier('add'), 'foo'), identifier('b'))),
                let_('two', clos({ 'foo': from(identifier('add'), 'foo')})),
                break_('$return', identifier('two'))
            ])
        ])),
        let_('toss', block([
            label('$return', [
                let_('b', number(1)),
                set(from(identifier('add'), 'foo'), binop('+', from(identifier('add'), 'foo'), identifier('b'))),
                let_('two', clos({ 'foo': from(identifier('add'), 'foo')})),
                break_('$return', identifier('two'))
            ])
        ])),
        let_('ret', block([
            label('$return', [
                let_('c', number(4)),
                set(from(identifier('sub'), 'foo'), binop('-', from(identifier('sub'), 'foo'), identifier('c'))),
                let_('three', clos({ 'foo': from(identifier('sub'), 'foo')})),
                break_('$return', identifier('three'))
            ])
        ])),
        let_('foo', block([
            label('$return', [
                break_('$return', from(identifier('ret'), 'foo'))
            ])
        ]))
    ]));
});