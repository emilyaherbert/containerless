import * as b from '@babel/types';
import { assertNormalized } from './assertNormalized';
import { Map } from 'immutable';
import * as parser from '@babel/parser';
import generator from '@babel/generator';
import * as n from '@stopify/normalize-js';

type State = Map<string, boolean>;

// NOTE(emily): This may not be righb. I have not yet hit a case where the error is triggered.
function merge(x: State, y: State): State {
    return x.mergeWith(
        (v1, v2) => {
            if(v1! !== v2!) {
                throw new Error("Mismatched kinds!");
            } else {
                return v1!;
            }
        },
        y
    )
}

const functionBreakName = '$return';

const t: b.MemberExpression = b.memberExpression(b.identifier('cb'), b.identifier('trace'));

const newTrace: b.ExpressionStatement =
    b.expressionStatement(
        b.callExpression(
            b.memberExpression(
                t,
                b.identifier('newTrace')
            ),
            []
        )
    );

const getTrace: b.ExpressionStatement =
    b.expressionStatement(
        b.callExpression(
            b.memberExpression(
                t,
                b.identifier('getTrace')
            ),
            []
        )
    );

function identifier(s: string): b.CallExpression {
    const callee = b.memberExpression(
        b.identifier('exp'),
        b.identifier('identifier')
    );
    const theArgs = [b.stringLiteral(s)];
    return b.callExpression(callee, theArgs);
}

function from(lhs: b.Identifier | b.CallExpression, rhs: string): b.CallExpression {
    const callee = b.memberExpression(
        b.identifier('exp'),
        b.identifier('from')
    );
    const theArgs = [lhs, b.stringLiteral(rhs)];
    return b.callExpression(callee, theArgs);
}

function number(n: number): b.CallExpression {
    const callee = b.memberExpression(
        b.identifier('exp'),
        b.identifier('number')
    );
    const theArgs = [b.numericLiteral(n)];
    return b.callExpression(callee, theArgs);
}

function boolean(bool: boolean): b.CallExpression {
    const callee = b.memberExpression(
        b.identifier('exp'),
        b.identifier('boolean')
    );
    const theArgs = [b.booleanLiteral(bool)];
    return b.callExpression(callee, theArgs);
}

function string(s: string): b.CallExpression {
    const callee = b.memberExpression(
        b.identifier('exp'),
        b.identifier('string')
    );
    const theArgs = [b.stringLiteral(s)];
    return b.callExpression(callee, theArgs);
}

function clos(fvs: b.ObjectProperty[]): b.CallExpression {
    const callee = b.memberExpression(
        b.identifier('exp'),
        b.identifier('clos')
    );
    const theArgs = [b.objectExpression(fvs)];
    return b.callExpression(callee, theArgs);
}

const undefined_: b.Identifier = b.identifier('undefined_');

function binop(op: string, e1: b.Expression, e2: b.Expression): b.CallExpression {
    const callee = b.memberExpression(
        b.identifier('exp'),
        b.identifier('binop')
    );
    const theArgs = [b.stringLiteral(op), e1, e2];
    return b.callExpression(callee, theArgs);
}

function traceLet(lhs: string, rhs: b.Expression): b.ExpressionStatement {
    const memberExpression = b.memberExpression(t, b.identifier('traceLet'));
    const callExpression = b.callExpression(memberExpression, [b.stringLiteral(lhs), rhs]);
    return b.expressionStatement(callExpression);
}

/**
 * ```
 * let [$clos] = b.traceFunctionBody('$return');
 * ```
 */
function jsLet(lhs: b.LVal, rhs: b.Expression): b.VariableDeclaration {
    const variableDeclarator = b.variableDeclarator(lhs, rhs);
    return b.variableDeclaration('let', [variableDeclarator]);
}

function traceSet(lhs: b.Expression, rhs: b.Expression): b.CallExpression {
    const memberExpression = b.memberExpression(t, b.identifier('traceSet'));
    return b.callExpression(memberExpression, [lhs, rhs]);
}

function traceWhile(test: b.Expression): b.ExpressionStatement {
    const memberExpression = b.memberExpression(t, b.identifier('traceWhile'));
    const callExpression = b.callExpression(memberExpression, [test]);
    return b.expressionStatement(callExpression);
}

const traceLoop: b.ExpressionStatement =
    b.expressionStatement(
        b.callExpression(
            b.memberExpression(
                t,
                b.identifier('traceLoop')
            ),
            []
        )
    );

function traceIfTrue(id: string): b.ExpressionStatement {
    const memberExpression = b.memberExpression(t, b.identifier('traceIfTrue'));
    const callExpression = b.callExpression(memberExpression, [b.identifier(id)]);
    return b.expressionStatement(callExpression);
}

function traceIfFalse(id: string): b.ExpressionStatement {
    const memberExpression = b.memberExpression(t, b.identifier('traceIfFalse'));
    const callExpression = b.callExpression(memberExpression, [b.identifier(id)]);
    return b.expressionStatement(callExpression);
}

// TODO(emily): Change this to LVal later.
function traceFunctionCall(id: string, theArgs: b.Expression[]): b.ExpressionStatement {
    const memberExpression = b.memberExpression(t, b.identifier('traceFunctionCall'));
    const memberArgs = [b.stringLiteral(id), b.arrayExpression(theArgs)];
    const callExpression = b.callExpression(memberExpression, memberArgs);
    return b.expressionStatement(callExpression);
}

function traceFunctionBody(): b.Expression {
    const memberExpression = b.memberExpression(t, b.identifier('traceFunctionBody'));
    return b.callExpression(memberExpression, [b.stringLiteral(functionBreakName)]);
}

function traceLabel(name: string): b.ExpressionStatement {
    const memberExpression = b.memberExpression(t, b.identifier('traceLabel'));
    const callExpression = b.callExpression(memberExpression, [b.stringLiteral(name)]);
    return b.expressionStatement(callExpression);
}

function traceBreak(name: string, value : b.Expression = undefined_): b.ExpressionStatement {
    const memberExpression = b.memberExpression(t, b.identifier('traceBreak'));
    const callExpression = b.callExpression(memberExpression, [b.stringLiteral(name), value]);
    return b.expressionStatement(callExpression);
}

const exitBlock: b.ExpressionStatement =
    b.expressionStatement(
        b.callExpression(
            b.memberExpression(
                t,
                b.identifier('exitBlock')
            ),
            []
        )
    );

/**
 * ```
 * x
 * ```
 * 
 * ```
 * identifier(x)
 * ```
 * 
 * ```
 * number(1)
 * ```
 * 
 * ```
 * boolean(true)
 * ```
 * 
 * ```
 * binop('+', number(1), number(2))
 * ```
 * ---
 * 
 * ```
 * foo = 12;
 * ```
 * 
 * ```
 * b.traceSet('foo', number(12));
 * foo = 12;
 * ```
 */
function reifyExpression(e: b.Expression, st: State): [b.Expression, State] {
    switch(e.type) {
        case 'Identifier': {
            if(st.has(e.name)) {
                if(st.get(e.name)) {
                    return [from(b.identifier('$clos'), e.name), st];
                } else {
                    return [identifier(e.name), st];
                }
            } else {
                return [from(b.identifier('$clos'), e.name), st.set(e.name, true)];
            }
        }
        case 'NumericLiteral': return [number(e.value), st];
        case 'BooleanLiteral': return [boolean(e.value), st];
        case 'StringLiteral': return [string(e.value), st];
        case 'BinaryExpression': {
            const [left, st1] = reifyExpression(e.left, st);
            const [right, st2] = reifyExpression(e.right, st);
            return [binop(e.operator, left, right), merge(st1, st2)];
        }
        case 'AssignmentExpression': {
            const [right, st1] = reifyExpression(e.right, st);
            // TODO(emily): This will need to be changed.
            const [left, st2] = reifyExpression(b.identifier(lvaltoName(e.left)), st);
            return [traceSet(left, right), merge(st1, st2)];
        }
        case 'MemberExpression': {
            // TODO(emily): It could be the case that obj is a FV.
            const obj = e.object;
            const prop = e.property;
            if(!b.isIdentifier(obj) || !b.isIdentifier(prop)) {
                console.log(e);
                throw new Error("Cannot chain member expressions!");
            }
            return [from(identifier(obj.name), prop.name), st];
        }
        default: return [b.stringLiteral('TODO: ' + e.type), st];
    }
}

/**
 * 
 * ```
 * b.traceLet('foo', number(1));
 * let foo = 1;
 * ```
 * 
 * ```
 * b.traceFunctionCall('foo', [identifier('F'), number(1)]);
 * let foo = F(1);
 * b.exitBlock();
 * ```
 * 
 * ```
 * b.traceLet('a', number(1));
 * let a = 1;
 * b.traceLet('F', clos({ a: identifier('a') }));
 * function F(x) { // let F = function(x) {
 *  let [$clos, $x] = b.traceFunctionBody('$return');
 *  b.traceLet('x', $x);
 *  b.traceBreak('$return', binop('+', from($clos, 'a'), identifier('x')));
 *  return a + x; * 
 *  b.exitBlock();
 * }
 * ```
 */
function reifyVariableDeclaration(s: b.VariableDeclaration, st: State): [b.Statement[], State] {
    let s1 = assertNormalized(s);
    const name = lvaltoName(s1.declarations[0].id);
    const init = s1.declarations[0].init;
    switch(init.type) {
        case 'CallExpression': {
            let init1 = assertNormalized(init);
            let theArgs: b.Expression[] = [];
            if(b.isIdentifier(init1.callee)) {
                if(init1.callee.name === 'require') {
                    return [[ s ], st];
                }
                theArgs.push(identifier(init1.callee.name));
            } else {
                const obj = init1.callee.object;
                const prop = init1.callee.property;
                if(!b.isIdentifier(obj) || !b.isIdentifier(prop)) {
                    throw new Error("Cannot chain member expressions!");
                }
                theArgs.push(from(identifier(obj.name), prop.name));
            }
            let nextSt = st;
            init1.arguments.forEach(a => {
                const [a1, st1] = reifyExpression(a, st);
                theArgs.push(a1);
                nextSt = merge(st1, nextSt);
            });
            const tCall = traceFunctionCall(name, theArgs);
            return [[tCall, s, exitBlock], nextSt.set(name, false)];
        }
        case 'FunctionExpression': {
            const newFun = b.functionDeclaration(b.identifier(name), init.params, init.body);
            return reifyFunctionDeclaration(newFun, st);
        }
        default: {
            const [init2, st1] = reifyExpression(init, st);
            const tLet = traceLet(name, init2);
            return [[tLet, s], st1.set(name, false)];
        }
    }
}

/**
 * ```
 * b.traceWhile(identifier('c'));
 * while(c) {
 *  b.traceLoop();
 *  ...
 * }
 * b.exitBlock();
 * ```
 */
function reifyWhileStatement(s: b.WhileStatement, st: State): [b.Statement[], State] {
    const [test, st1] = reifyExpression(s.test, st);
    let [body, st2] = reifyStatement(s.body, st);
    body.unshift(traceLoop);
    const tWhile = traceWhile(test);
    const theWhile = b.whileStatement(s.test, b.blockStatement(body));
    return [[tWhile, theWhile, exitBlock], merge(st1, st2)];
}

/**
 * 
 * ```
 * let $test = identifier(c);
 * if(c) {
 *  b.traceIfTrue($test);
 *  ...
 * } else {
 *  b.traceIfFalse($test);
 * }
 * b.exitBlock();
 * ```
 * 
 */
function reifyIfStatement(s: b.IfStatement, st: State): [b.Statement[], State] {
    const [test, st1] = reifyExpression(s.test, st);
    let [ifTrue, st2] = reifyStatement(s.consequent, st);
    let [ifFalse, st3]: [b.Statement[], State] = [[], st];
    if(s.alternate !== null) {
        [ifFalse, st3] = reifyStatement(s.alternate, st);
    }
    const id = '$test';
    ifTrue.unshift(traceIfTrue(id));
    ifFalse.unshift(traceIfFalse(id));
    const tTest = jsLet(b.identifier(id), test);
    const theIf = b.ifStatement(s.test, b.blockStatement(ifTrue), b.blockStatement(ifFalse));
    return [[tTest, theIf, exitBlock], merge(merge(st1, st2), st3)];
}

function reifyExpressionStatement(s: b.ExpressionStatement, st: State): [b.Statement[], State] {
    const [expression, st1] = reifyExpression(s.expression, st);
    const above = b.expressionStatement(expression);
    return [[above, s], st1];
}

/**
 * 
 * ```
 * b.traceLabel('l');
 * l: {
 *  ...
 *  b.traceBreak('l');
 *  break l;
 *  b.exitBlock();
 * }
 * ```
 */
function reifyLabeledStatement(s: b.LabeledStatement, st: State): [b.Statement[], State] {
    const name = s.label;
    let [body, st1] = reifyStatement(s.body, st);
    body.push(exitBlock);
    const tLabel = traceLabel(lvaltoName(name));
    const theLabel = b.labeledStatement(name, b.blockStatement(body));
    return [[tLabel, theLabel], st1];
}

/**
 * ```
 * b.traceBreak('l');
 * break l;
 * ```
 */
function reifyBreakStatement(s: b.BreakStatement, st: State): [b.Statement[], State] {
    const name = s.label;
    if(name === null) {
        throw new Error("Found null label in break.");
    } else {
        const tBreak = traceBreak(lvaltoName(name));
        return [[tBreak, s], st];
        // TODO(emily): wrong ?
    }
}

/**
 * 
 * ```
 * b.traceLet('a', number(1));
 * let a = 1;
 * b.traceLet('F', clos({ a: identifier('a') }));
 * function F(x) {
 *  let [$clos, $x] = b.traceFunctionBody('$return');
 *  b.traceLet('x', $x);
 *  b.traceBreak('$return', binop('+', from($clos, 'a'), identifier('x')));
 *  return a + x; * 
 *  b.exitBlock();
 * }
 * ```
 */
function reifyFunctionDeclaration(s: b.FunctionDeclaration, st: State): [b.Statement[], State] {
    const id = s.id;
    if(id === null) {
        throw new Error("Null id!!");
    }
    const params = s.params;
    let funBodyLHS = [b.identifier('$clos')];
    let paramsBody: b.Statement[] = [];
    let nextSt: State = Map();
    for(let i=0; i<params.length; i++) {
        const p = params[i];
        if(!b.isIdentifier(p)) {
            throw new Error("Expected identitifier!");
        } else {
            const oldName = lvaltoName(p);
            const newName = b.identifier('$' + oldName);
            funBodyLHS.push(newName);
            paramsBody.push(traceLet(oldName, newName));
            nextSt = nextSt.set(oldName, false);
        }
    }
    let [body, myState] = reifyStatement(s.body, nextSt);
    body = paramsBody.concat(body);
    body.unshift(jsLet(b.arrayPattern(funBodyLHS), traceFunctionBody()));
    body.push(exitBlock); // exit the label
    const retSt = st.set(lvaltoName(id), false);
    let fvs: b.ObjectProperty[] = [];
    myState.filter(v => v!)
        .keySeq().forEach(k => {
            if(retSt.has(k!)) {
                if(retSt.get(k!)!) {
                    fvs.push(b.objectProperty(b.identifier(k!), from(b.identifier('$clos'), k!)));
                } else {
                    fvs.push(b.objectProperty(b.identifier(k!), identifier(k!)));
                }
            } else {
                throw new Error("Not found!");
            }
        });
    const tClos = traceLet(lvaltoName(id), clos(fvs));
    const theFunction = b.functionDeclaration(id, params, b.blockStatement(body));
    return [[tClos, theFunction], retSt];
}

/**
 * ```
 * b.traceBreak('$return', number(42));
 * return 42;
 * ```
 */
function reifyReturnStatement(s_: b.ReturnStatement, st: State): [b.Statement[], State] {
    let s = assertNormalized(s_);
    const [argument, st1] = reifyExpression(s.argument, st);
    const tBreak = traceBreak(functionBreakName, argument);
    return [[tBreak, s], st1];
}

function reifyStatement(s: b.Statement, st: State): [b.Statement[], State] {
    switch(s.type) {
        case 'VariableDeclaration': return reifyVariableDeclaration(s, st);
        case 'WhileStatement': return reifyWhileStatement(s, st);
        case 'BlockStatement': return reifyStatements(s.body, st); // NOTE: this unwraps block statements.
        case 'IfStatement': return reifyIfStatement(s, st);
        case 'ExpressionStatement': return reifyExpressionStatement(s, st);
        case 'LabeledStatement': return reifyLabeledStatement(s, st);
        case 'BreakStatement': return reifyBreakStatement(s, st);
        case 'FunctionDeclaration': return reifyFunctionDeclaration(s, st);
        case 'ReturnStatement': return reifyReturnStatement(s, st);
        // TODO(arjun): Dangerous! Remove before any experiments.
        default: return [[b.expressionStatement(b.stringLiteral('TODO: ' + s.type))], st];
    }
}

function reifyStatements(s: b.Statement[], st: State): [b.Statement[], State] {
    let ret: b.Statement[] = [];
    let nextSt = st;

    for(let i=0; i<s.length; i++) {
        let [r, st1] = reifyStatement(s[i], nextSt);
        for(let j=0; j<r.length; j++) {
            ret.push(r[j]);
        }
        nextSt = merge(nextSt, st1);
    }

    return [ret, nextSt];
}

function reify(s: b.Statement[]): b.Statement[] {
    const [ret, _] = reifyStatements(s, Map());
    //ret.unshift(jsLet(b.identifier('t'), newTrace));
    // TODO(emily): Fix. Need to detect require statements or something.
    ret.splice(1, 0, newTrace);
    ret.splice(1, 0, jsLet(b.identifier('exp'), b.memberExpression(b.identifier('containerless00'), b.identifier('exp'))));
    ret.splice(1, 0, jsLet(b.identifier('cb'), b.memberExpression(b.identifier('containerless00'), b.identifier('cb'))));
    ret.push(exitBlock);
    ret.push(getTrace);
    return ret;
}

export function transform(inputCode: string): string {
    let normalized = n.normalize(inputCode);
    let ast = parser.parse(normalized);

    ast.program.body = reify(ast.program.body);
    return generator(ast.program).code;
}

/*
export function testTransform(inputCode: string): any {
    //let normalized = n.normalize(inputCode);
    let ast = parser.parse(inputCode);

    ast.program.body = reify(ast.program.body);
    ast.program.body.push(getTrace);
    return generator(ast.program).code;
}
*/

/**
 * Given an 'LVal' that is an identifier, produces the identifier's name.
 * Throws an exception if the 'LVal' is not an identifier.
 *
 * @param lval an l-value
 * @returns the name of the identifier, if 'lval' is an identifier
 */
function lvaltoName(lval: b.LVal): string {
    if (b.isIdentifier(lval)) {
        return lval.name;
    } else if (lval.type === 'RestElement' && lval.argument.type === 'Identifier') {
        return lval.argument.name;
    } else {
        throw new Error(`Expected Identifier, received ${lval.type}`);
    }
}



/*

    Variables conditions:

    1. If free, use `from($clos, 'foo')`
    2. Else, use `identifier('foo')`

*/