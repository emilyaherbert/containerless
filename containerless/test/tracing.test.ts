import {
    while_, break_, label, block, let_, set_, number, if_, newTrace, callback,
    identifier, string, binop, unknown, undefined_, clos, from
} from '../ts/tracing';
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

test('tracing a function', () => {
    let t = newTrace();

    function F(x: any) {
        t.traceLet('x', t.popArg());
        t.traceLet('y', binop('+', identifier('x'), number(10)));
        let y = x + 10;
        t.traceReturn(identifier('y'));
        return y;
    }

    let a = 100;
    t.traceLet('a', number(100));
    t.pushArg(identifier('a'));
    t.traceNamed('w');
    let w = F(a);
    t.exitBlock();

    t.traceLet('z', number(200));
    t.pushArg(identifier('z'));
    t.traceNamed('v');
    let v = F(200);
    t.exitBlock();

    t.exitBlock();

    expect(t.getTrace()).toMatchObject(block([
        let_('a', number(100)),
        let_('w', block([
            let_('x', identifier('a')),
             let_('y', binop('+', identifier('x'), number(10))),
             identifier('y')
            ])),
        let_('z', number(200)),
        let_('v', block([
            let_('x', identifier('z')),
                let_('y', binop('+', identifier('x'), number(10))),
                identifier('y')
            ])),
        ]));
});

test('same fun, different control flow', () => {
    let t = newTrace();

    function F(x: any) {
        t.traceLet('x', t.popArg());
        t.traceLet('ret', number(0));
        let ret = 0;
        if(x > 10) {
            t.traceIfTrue(binop('>', identifier('x'), number(10)));
            t.traceSet('ret', number(42));
            ret = 42;
        } else {
            t.traceIfFalse(binop('>', identifier('x'), number(10)));
            t.traceSet('ret', number(24));
            ret = 24;
        }
        t.exitBlock();

        t.traceReturn(identifier("ret"));
        return ret;
    }

    let a = 11;
    t.traceLet('a', number(11));
    t.pushArg(identifier('a'));
    t.traceNamed('w');
    let w = F(a);
    t.exitBlock();

    let b = 9;
    t.traceLet('b', number(9));
    t.pushArg(identifier('b'));
    t.traceNamed('v');
    let v = F(b);
    t.exitBlock();

    t.exitBlock();

    expect(t.getTrace()).toMatchObject(block([
        let_('a', number(11)),
        let_('w', block([
             let_('x', identifier('a')),
             let_('ret', number(0)),
             if_(binop('>', identifier('x'), number(10)),
                [set_('ret', number(42))],
                [unknown()]),
             identifier("ret")
            ])),
        let_('b', number(9)),
        let_('v', block([
             let_('x', identifier('b')),
             let_('ret', number(0)),
             if_(binop('>', identifier('x'), number(10)),
                [unknown()],
                [set_('ret', number(24))]),
             identifier("ret")
            ])),
        ]));
});

test('exit fun from within if', () => {
    let t = newTrace();

    function F(x: any) {
        t.traceLabel('$return');
        t.traceLet('x', t.popArg());

        if(x > 10) {
            t.traceIfTrue(binop('>', identifier('x'), number(10)));
            t.traceBreak('$return', number(42));
            return 42;
        } else {
            t.traceIfFalse(binop('>', identifier('x'), number(10)));
            t.traceBreak('$return', number(24));
            return 24;
        }
        t.exitBlock();
        t.exitBlock();
    }

    let a = 11;
    t.traceLet('a', number(11));
    t.pushArg(identifier('a'));
    t.traceNamed('w');
    let w = F(a);
    t.exitBlock();

    let b = 9;
    t.traceLet('b', number(9));
    t.pushArg(identifier('b'));
    t.traceNamed('v');
    let v = F(b);
    t.exitBlock();

    t.exitBlock();

    expect(t.getTrace()).toMatchObject(block([
        let_('a', number(11)),
        let_('w', block([
             label('$return',
                [ let_('x', identifier('a')),
                  if_(binop('>', identifier('x'), number(10)),
                    [ break_('$return', number(42)) ],
                    [ unknown() ]),
                  unknown()
                ])
            ])),
        let_('b', number(9)),
        let_('v', block([
             label('$return',
                [ let_('x', identifier('b')),
                  if_(binop('>', identifier('x'), number(10)),
                    [ unknown() ],
                    [ break_('$return', number(24)) ]),
                  unknown()
                ])
            ])),
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
            t.traceSet('y', binop('+', identifier('y'), number(2)));
            y = y + 2;
        } else {
            t.traceIfFalse(binop('>', identifier('x'), number(1)));
            t.traceSet('y', binop('+', identifier('y'), number(3)));
            y = y + 3;
        }
        t.exitBlock();

        t.traceSet("x", binop("-", identifier("x"), number(1)));
        x = x - 1;
    };
    t.exitBlock();
    t.exitBlock();

    expect(t.getTrace()).toMatchObject(block([
        let_('x', number(2)),
        let_('y', number(0)),
        while_(binop('>', identifier('x'), number(0)),
            [ if_(binop('>', identifier('x'), number(1)),
                  [ set_('y', binop('+', identifier('y'), number(2))) ],
                  [ set_('y', binop('+', identifier('y'), number(3))) ]),
              set_('x', binop('-', identifier('x'), number(1)))
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
            cb.trace.traceSet('ret', number(200));
            ret = 200;
        }
        else {
            cb.trace.traceIfFalse($cond);
            cb.trace.traceSet('ret', number(-200));
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
                    set_('ret', number(200))
                ], [
                    set_('ret', number(-200))
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
        t.traceSet('y', number(10));
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
        t.traceSet('y', number(10));
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
            [ set_('y', number(10)) ],
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
        t.traceSet('x', number(200));
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
        t.traceSet('x', number(200));
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
                    set_('x', number(200))
                ])
        ]));
});


test('make adder', () => {
    let t = newTrace();

    t.traceLet('first', number(1));
    let first = 1;

    let $c0 = t.traceClos('make_adder');
    function make_adder(a: any) {
        t.traceFunctionBody(['a'], $c0); // creates '$return' label
 
            t.traceLet('second', number(2));
            let second = 2;
 
            let $c1 = t.traceClos('add');
            function add(b: any) {
                t.traceFunctionBody(['b'], $c1);

                    t.traceBreak('$return', binop('+', t.traceIdentifier('a'), t.traceIdentifier('b')));
                    return a + b;
 
                t.exitBlock();
            };
           
            t.traceBreak('$return', t.traceIdentifier('add'));
            return add;
        t.exitBlock();
    };
 
    t.traceFunctionCall('F', 'make_adder', [number(9)]);
    let F = make_adder(9);
    t.exitBlock();

    t.traceFunctionCall('res1', 'F', [number(5)]);
    let res1 = F(5);
    t.exitBlock();
 
    t.traceFunctionCall('res2', 'F', [number(2)]);
    let res2 = F(2);
    t.exitBlock();
 
    t.traceLet('res', binop('+', t.traceIdentifier('res1'), t.traceIdentifier('res2')));
    let res = res1 + res2;   
   
    t.exitBlock();
    t.prettyPrint();
 
    expect(t.getTrace()).toMatchObject(
        undefined_
    );
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
    t.prettyPrint();

    expect(t.getTrace()).toMatchObject(
        undefined_
    );
 });

 test('make adder 3', () => {
    let t = newTrace();

    t.traceLet('make_adder', clos({ } as any));
    function make_adder(a: any) {
        let [$clos, $a] = t.traceFunctionBody2('$return');
        
        t.traceLet('add', clos({ 'a':  $a } as any));
        function add(b: any) {
            let [$clos, $b] = t.traceFunctionBody2('$return');
            let $a = from([ $clos as any, 'a']);
            t.traceBreak('$return', binop('+', $a, $b));
        };

        t.traceBreak('$return', identifier('add'));
        return add;

    };

    t.traceFunctionCall2('F', [identifier('make_adder'), number(9)]);
    let F = make_adder(9);
    t.exitBlock();

    t.traceFunctionCall2('res1', [identifier('F'), number(5)]);
    let res1 = F(5);
    t.exitBlock();

    t.exitBlock(); // end the turn
    t.prettyPrint();

    expect(t.getTrace()).toMatchObject(
        undefined_
    );
 });