import {
    while_, break_, label, block, let_, set, number, if_, callback,
    identifier, string, binop, unknown, undefined_, clos, from, froms
} from '../ts/exp';
import {
    newTrace
} from '../ts/tracing'
import { Callbacks } from '../ts/callbacks';

test('trivial trace', () => {
    let t = newTrace();
    t.traceLet('x', number(12));
    t.traceLet('y', number(120));
    t.exitBlock();
    expect(t.getTrace()).toMatchObject(
        block([
            let_('x', number(12)),
            let_('y', number(120))]));
});

test('trivial retrace', () => {
    let t = newTrace();
    t.traceLet('x', number(12));
    t.traceLet('y', number(120));
    t.exitBlock();
    t.newTrace();
    t.traceLet('x', number(12));
    t.traceLet('y', number(120));
    t.exitBlock();
    expect(t.getTrace()).toMatchObject(
        block([
            let_('x', number(12)),
            let_('y', number(120))]));
});

test('tracing both branches of a conditional', () => {
    let t = newTrace();
    t.traceIfTrue(number(0));
    t.traceLet('x', number(100));
    t.exitBlock();
    t.traceLet('z', number(300));
    t.exitBlock();

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
    let innerTrace = t.traceCallback('dummy-event', identifier('x'), 'x');
    t.traceLet('y', number(20));
    t.exitBlock();

    // Now, the callback runs
    innerTrace.traceLet('z', number(30));
    innerTrace.exitBlock();

    expect(t.getTrace()).toMatchObject(
        block([
            let_('x', number(10)),
            callback('dummy-event', identifier('x'), 'x', [
                let_('z', number(30))]),
            let_('y', number(20))]));
});

test('same fun, different control flow', () => {
    let t = newTrace();

    t.traceLet('F', clos({ } as any));
    function F(x: any) {
        let [$clos, $x] = t.traceFunctionBody('$return');

        t.traceLet('ret', number(0));
        let ret = 0;
        let $cond = binop('>', $x, number(10));
        if(x > 10) {
            t.traceIfTrue($cond);
            t.traceSet([ 'ret' ], number(42));
            ret = 42;
        } else {
            t.traceIfFalse($cond);
            t.traceSet([ 'ret' ], number(24));
            ret = 24;
        }
        t.exitBlock();

        t.traceBreak('$return', identifier('ret'));
        return ret;
    }

    t.traceFunctionCall('w', [identifier('F'), number(11)]);
    let w = F(11);
    t.exitBlock();

    t.traceFunctionCall('v', [identifier('F'), number(9)]);
    let v = F(9);
    t.exitBlock();

    t.exitBlock();
    //t.prettyPrint();

    expect(t.getTrace()).toMatchObject(block([
        let_('F', clos({ } as any)),
        let_('w', block(
            [
                label('$return', 
                    [
                        let_('ret', number(0)),
                        if_(binop('>', number(11), number(10)),
                            [ set(['ret'], number(42)) ],
                            [ unknown() ]),
                        break_('$return', identifier('ret'))
                    ])
            ])),
        let_('v', block(
            [
                label('$return', 
                    [
                        let_('ret', number(0)),
                        if_(binop('>', number(9), number(10)),
                            [ unknown() ],
                            [ set(['ret'], number(24)) ]),
                        break_('$return', identifier('ret'))
                    ])
            ])),
        ]));
});

test('exit fun from within if', () => {
    let t = newTrace();

    t.traceLet('F', clos({ } as any));
    function F(x: any) {
        let [$clos, $x] = t.traceFunctionBody('$return');

        let $cond = binop('>', $x, number(10));
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
        let_('F', clos({ } as any)),
        let_('w', block(
            [
                label('$return', 
                    [
                        if_(binop('>', number(11), number(10)),
                            [ break_('$return', number(42)) ],
                            [ unknown() ]),
                        unknown()
                    ])
            ])),
        let_('v', block(
            [
                label('$return', 
                    [
                        if_(binop('>', number(9), number(10)),
                            [ unknown() ],
                            [ break_('$return', number(24)) ]),
                        unknown()
                    ])
            ]))
        ]));
});

test('tracing with callback library', (done) => {
    let cb = new Callbacks();
    cb.immediate('hello', (str) => {
        cb.trace.traceLet('x', number(100));
        // Why is this in here? If we put it after the last line
        // (cb.trace.exitBlock), we will get the trace before this callback
        // is called. If we don't wrap it in setImmediate, we will get it
        // when cb.trace refers to the inner trace.
        setImmediate(() => {
            expect(cb.trace.getTrace()).toMatchObject(
                block([
                    callback('immediate', string('hello'), '$x', [
                        let_('x', number(100))]),
                    let_('y', number(200))]));
            done();
        });
    });
    cb.trace.traceLet('y', number(200));
    cb.trace.exitBlock();

});

test('while', () => {
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
            t.traceSet([ 'y' ], binop('+', identifier('y'), number(2)));
            y = y + 2;
        } else {
            t.traceIfFalse(binop('>', identifier('x'), number(1)));
            t.traceSet([ 'y' ], binop('+', identifier('y'), number(3)));
            y = y + 3;
        }
        t.exitBlock();

        t.traceSet([ 'x' ], binop("-", identifier("x"), number(1)));
        x = x - 1;
    };
    t.exitBlock();
    t.exitBlock();

    expect(t.getTrace()).toMatchObject(block([
        let_('x', number(2)),
        let_('y', number(0)),
        while_(binop('>', identifier('x'), number(0)),
            [ if_(binop('>', identifier('x'), number(1)),
                  [ set([ 'y' ], binop('+', identifier('y'), number(2))) ],
                  [ set([ 'y' ], binop('+', identifier('y'), number(3))) ]),
              set([ 'x' ], binop('-', identifier('x'), number(1)))
            ])
        ]));
});

test('callback that receives multiple events', () => {
    let cb = new Callbacks();
    let sender = cb.mockCallback((value) => {
        let $value = cb.trace.popArg();
        cb.trace.traceLet('value', $value);
        cb.trace.traceLet('ret', number(0));
        let ret = 0;
        let $cond = binop('>', identifier('value'), number(0));
        if (value > 0) {
            cb.trace.traceIfTrue($cond);
            cb.trace.traceSet([ 'ret' ], number(200));
            ret = 200;
        }
        else {
            cb.trace.traceIfFalse($cond);
            cb.trace.traceSet([ 'ret' ], number(-200));
            ret = -200;
        }
        // exit the true/false parts, not the function
        cb.trace.exitBlock();
    });
    cb.trace.exitBlock(); // end of the first turn
    sender(-100); // callback invoked
    sender(100); // callback invoked
    expect(cb.trace.getTrace()).toMatchObject(
        block([
            callback('mock', number(0), '$response', [
                let_('value', identifier('$response')),
                let_('ret', number(0)),
                if_(binop('>', identifier('value'), number(0)), [
                    set([ 'ret' ], number(200))
                ], [
                    set([ 'ret' ], number(-200))
                ])
            ])
        ]));
});

test('label and break', () => {
    let t = newTrace();

    t.traceLabel('things');
    things : {
        t.traceLet('x', number(77));
        let x = 77;
        t.traceBreak('things', undefined_);
        break things;
        t.traceLet('oops', number(666));
        let oops = 666;
        t.exitBlock();
    }


    t.traceLet('keyboard', number(11));
    let keyboard = 11;

    t.exitBlock();

    expect(t.getTrace()).toMatchObject(block([
        label('things',
            [ let_('x', number(77)),
              break_('things', undefined_)
            ]),
        let_('keyboard', number(11))
        ]));

});

test('label no break', () => {
    let t = newTrace();

    t.traceLabel('things');
    things : {
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
        label('things',
            [ let_('x', number(77)),
              let_('oops', number(666))
            ]),
        let_('keyboard', number(11))
        ]));

});

test('nested labels', () => {
    let t = newTrace();
    
    t.traceLabel('things');
    things : {
        t.traceLet('x', number(1));
        let x = 1;

        t.traceLabel('betwixt');
        betwixt : {
            t.traceLet('y', number(2));
            let y = 2;

            t.traceLabel('stuff');
            stuff : {
                t.traceLet('z', number(3));
                let z = 3;
                t.traceBreak('betwixt', undefined_);
                break betwixt;
                t.exitBlock();
            }

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
        label('things',
            [ let_('x', number(1)),
              label('betwixt',
                [ let_('y', number(2)),
                  label('stuff',
                    [ let_('z', number(3)),
                      break_('betwixt', undefined_)
                    ]),
                  unknown()
                ]),
              let_('here', number(51))
            ]),
        let_('after', number(80))
        ]));

});

test('label and if and break', () => {
    let t = newTrace();
    t.traceLabel('things');
    things : {
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

    t.newTrace();
    t.traceLabel('things');
    things : {
        t.traceLet('x', number(77)); // kept at 77 for testing
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
        label('things',
            [ let_('x', number(77)),
              if_(binop('>', identifier('x'), number(10)),
                 [ let_('y', number(333)),
                   break_('things', undefined_)
                 ],
                 [ let_('y', number(444)),
                   break_('things', undefined_)
                 ]),
              unknown()
            ]),
        let_('keyboard', number(11))
        ]));

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
        t.traceSet([ 'y' ], number(10));
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
        t.traceSet([ 'y' ], number(10));
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
            [ set([ 'y' ], number(10)) ],
            [ ]),
        let_('z', number(5))
        ]));
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
        t.traceSet([ 'x' ], number(200));
        x = 200;
        t.exitBlock();
    }

    t.newTrace();
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
        t.traceSet([ 'x' ], number(200));
        x = 200;
        t.exitBlock();
    }    

    t.exitBlock();
    expect(t.getTrace()).toMatchObject(
        block([
            let_('x', number(1)),
            label('myLabel',
                [
                    if_(binop('>', identifier('x'), number(0)),
                        [ break_('myLabel', undefined_) ],
                        []),
                    set([ 'x' ], number(200))
                ])
        ]));
});

 test('make adder 2', () => {
    let t = newTrace();

    t.traceLet('make_adder', clos({ } as any));
    function make_adder(a: any) {
        t.traceLabel('$return');
        let $clos = t.popArg();
        let $a = t.popArg();
        t.traceLet('add', clos({ 'a':  $a } as any));
        function add(b: any) {
            t.traceLabel('$return');
            let $clos = t.popArg();
            let $b = t.popArg();
            let $a = from([ $clos as any, 'a']);
            t.traceBreak('$return', binop('+', $a, $b));
        }
        t.traceBreak('$return', identifier('add'));
        return add;

    };

    t.pushArg(number(9));
    t.pushArg(identifier('make_adder'));
    t.traceNamed('F');
    let F = make_adder(9);
    t.exitBlock();

    t.pushArg(number(5));
    t.pushArg(identifier('F'));
    t.traceNamed('res1');
    let res1 = F(5);
    t.exitBlock();

    t.exitBlock(); // end the turn
    //t.prettyPrint();

    expect(t.getTrace()).toMatchObject(
        undefined_
    );
 });

 test('make adder 3', () => {
    let t = newTrace();

    t.traceLet('make_adder', clos({ } as any));
    function make_adder(a: any) {
        let [$clos, $a] = t.traceFunctionBody('$return')

        t.traceLet('add', clos({ 'a':  $a } as any));
        function add(b: any) {
            let [$clos, $b] = t.traceFunctionBody('$return')
            let [$a] = froms($clos as any, ['a']);
            t.traceBreak('$return', binop('+', $a, $b));
        };

        t.traceBreak('$return', identifier('add'));
        return add;

    };

    t.traceFunctionCall('F', [identifier('make_adder'), number(9)]);
    let F = make_adder(9);
    t.exitBlock();

    t.traceFunctionCall('res1', [identifier('F'), number(5)]);
    let res1 = F(5);
    t.exitBlock();

    t.exitBlock(); // end the turn
    //t.prettyPrint();

    expect(t.getTrace()).toMatchObject(
        undefined_
    );
 });

 test('crazy closures', () => {
    let t = newTrace();

    t.traceLet('zero', clos({ } as any));
    function zero() {
        let [$clos] = t.traceFunctionBody('$return')

        t.traceLet('foo', number(0));
        let foo = 0;
        
        t.traceLet('one', clos({ 'foo': 'foo' } as any));
        function one(b: any) {
            let [$clos, $b] = t.traceFunctionBody('$return')
            let [$foo] = froms($clos as any, ['foo']);

            t.traceSet([ $clos as any, 'foo'], binop('+', $foo, $b));
            foo = foo + b;
        
            t.traceLet('two', clos({ 'foo': $foo } as any));
            function two(c: any) {
                let [$clos, $c] = t.traceFunctionBody('$return')
                let [$foo] = froms($clos as any, ['foo']);

                t.traceSet([ $clos as any, 'foo'], binop('-', $foo, $c));
                foo = foo - c;
            
                t.traceLet('three', clos({ 'foo': $foo } as any));
                function three() {
                    let [$clos] = t.traceFunctionBody('$return')
                    let [$foo] = froms($clos as any, ['foo']);

                    t.traceBreak('$return', $foo);
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
    t.prettyPrint();

    expect(t.getTrace()).toMatchObject(
        undefined_
    );
 });