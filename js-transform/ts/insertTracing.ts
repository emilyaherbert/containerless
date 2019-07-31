import * as t from '@babel/types';
import { assertNormalized } from './assertNormalized';
import { Map } from 'immutable';
import * as parser from '@babel/parser';
import generator from '@babel/generator';
import * as n from '@stopify/normalize-js';

type State = Map<string, boolean>;

// NOTE(emily): This may not be right. I have not yet hit a case where the error is triggered.
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

const newTrace: t.Expression =
    t.callExpression(
        t.memberExpression(
            t.identifier('t'),
            t.identifier('newTrace')
        ),
        []
    );

function identifier(s: string): t.CallExpression {
    const callee = t.identifier('identifier');
    const theArgs = [t.stringLiteral(s)];
    return t.callExpression(callee, theArgs);
}

function from(lhs: t.Identifier, rhs: string): t.CallExpression {
    const callee = t.identifier('identifier');
    const theArgs = [lhs, t.stringLiteral(rhs)];
    return t.callExpression(callee, theArgs);
}

function number(n: number): t.CallExpression {
    const callee = t.identifier('number');
    const theArgs = [t.numericLiteral(n)];
    return t.callExpression(callee, theArgs);
}

function boolean(b: boolean): t.CallExpression {
    const callee = t.identifier('boolean');
    const theArgs = [t.booleanLiteral(b)];
    return t.callExpression(callee, theArgs);
}

function clos(fvs: t.ObjectProperty[]): t.CallExpression {
    const callee = t.identifier('clos');
    const theArgs = [t.objectExpression(fvs)];
    return t.callExpression(callee, theArgs);
}

const undefined_: t.Identifier = t.identifier('undefined_');

function binop(op: string, e1: t.Expression, e2: t.Expression): t.CallExpression {
    const callee = t.identifier('binop');
    const theArgs = [t.stringLiteral(op), e1, e2];
    return t.callExpression(callee, theArgs);
}

function traceLet(lhs: string, rhs: t.Expression): t.ExpressionStatement {
    const memberExpression = t.memberExpression(t.identifier('t'), t.identifier('traceLet'));
    const callExpression = t.callExpression(memberExpression, [t.stringLiteral(lhs), rhs]);
    return t.expressionStatement(callExpression);
}

/**
 * ```
 * let [$clos] = t.traceFunctionBody('$return');
 * ```
 */
function jsLet(lhs: t.LVal, rhs: t.Expression): t.VariableDeclaration {
    const variableDeclarator = t.variableDeclarator(lhs, rhs);
    return t.variableDeclaration('let', [variableDeclarator]);
}

// TODO(emily): LVal?
function traceSet(id: string, rhs: t.Expression): t.CallExpression {
    const memberExpression = t.memberExpression(t.identifier('t'), t.identifier('traceSet'));
    return t.callExpression(memberExpression, [identifier(id), rhs]);
}

function traceWhile(test: t.Expression): t.ExpressionStatement {
    const memberExpression = t.memberExpression(t.identifier('t'), t.identifier('traceWhile'));
    const callExpression = t.callExpression(memberExpression, [test]);
    return t.expressionStatement(callExpression);
}

const traceLoop: t.ExpressionStatement =
    t.expressionStatement(
        t.callExpression(
            t.memberExpression(
                t.identifier('t'),
                t.identifier('traceLoop')
            ),
            []
        )
    );

function traceIfTrue(id: string): t.ExpressionStatement {
    const memberExpression = t.memberExpression(t.identifier('t'), t.identifier('traceIfTrue'));
    const callExpression = t.callExpression(memberExpression, [t.identifier(id)]);
    return t.expressionStatement(callExpression);
}

function traceIfFalse(id: string): t.ExpressionStatement {
    const memberExpression = t.memberExpression(t.identifier('t'), t.identifier('traceIfFalse'));
    const callExpression = t.callExpression(memberExpression, [t.identifier(id)]);
    return t.expressionStatement(callExpression);
}

// TODO(emily): Change this to LVal later.
function traceFunctionCall(id: string, theArgs: t.Expression[]): t.ExpressionStatement {
    const memberExpression = t.memberExpression(t.identifier('t'), t.identifier('traceFunctionCall'));
    const memberArgs = [t.stringLiteral(id), t.arrayExpression(theArgs)];
    const callExpression = t.callExpression(memberExpression, memberArgs);
    return t.expressionStatement(callExpression);
}

function traceFunctionBody(): t.Expression {
    const memberExpression = t.memberExpression(t.identifier('t'), t.identifier('traceFunctionBody'));
    return t.callExpression(memberExpression, [t.stringLiteral(functionBreakName)]);
}

function traceLabel(name: string): t.ExpressionStatement {
    const memberExpression = t.memberExpression(t.identifier('t'), t.identifier('traceLabel'));
    const callExpression = t.callExpression(memberExpression, [t.stringLiteral(name)]);
    return t.expressionStatement(callExpression);
}

function traceBreak(name: string, value : t.Expression = undefined_): t.ExpressionStatement {
    const memberExpression = t.memberExpression(t.identifier('t'), t.identifier('traceBreak'));
    const callExpression = t.callExpression(memberExpression, [t.stringLiteral(name), value]);
    return t.expressionStatement(callExpression);
}

const exitBlock: t.ExpressionStatement =
    t.expressionStatement(
        t.callExpression(
            t.memberExpression(
                t.identifier('t'),
                t.identifier('exitBlock')
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
 * t.traceSet('foo', number(12));
 * foo = 12;
 * ```
 */
function reifyExpression(e: t.Expression, st: State): [t.Expression, State] {
    switch(e.type) {
        case 'Identifier': {
            if(st.has(e.name)) {
                if(st.get(e.name)) {
                    return [from(t.identifier('$clos'), e.name), st];
                } else {
                    return [identifier(e.name), st];
                }
            } else {
                return [from(t.identifier('$clos'), e.name), st.set(e.name, true)];
            }
        }
        case 'NumericLiteral': return [number(e.value), st];
        case 'BooleanLiteral': return [boolean(e.value), st];
        case 'BinaryExpression': {
            const [left, st1] = reifyExpression(e.left, st);
            const [right, st2] = reifyExpression(e.right, st);
            return [binop(e.operator, left, right), merge(st1, st2)];
        }
        case 'AssignmentExpression': {
            const [right, st1] = reifyExpression(e.right, st);
            return [traceSet(lvaltoName(e.left), right), st1];
        }
        default: return [t.stringLiteral('TODO: ' + e.type), st];
    }
}

/**
 * 
 * ```
 * t.traceLet('foo', number(1));
 * let foo = 1;
 * ```
 * 
 * ```
 * t.traceFunctionCall('foo', [identifier('F'), number(1)]);
 * let foo = F(1);
 * t.exitBlock();
 * ```
 */
function reifyVariableDeclaration(s: t.VariableDeclaration, st: State): [t.Statement[], State] {
    let s1 = assertNormalized(s);
    const name = lvaltoName(s1.declarations[0].id);
    const init = s1.declarations[0].init;
    switch(init.type) {
        case 'CallExpression': {
            let init1 = assertNormalized(init);
            const callee = init1.callee.name;
            let theArgs: t.Expression[] = [identifier(callee)];
            let nextSt = st;
            init1.arguments.forEach(a => {
                const [a1, st1] = reifyExpression(a, st);
                theArgs.push(a1);
                nextSt = merge(st1, nextSt);
            });
            const tCall = traceFunctionCall(name, theArgs);
            return [[tCall, s, exitBlock], nextSt.set(name, false)];
        }
        default: {
            const [init2, st1] = reifyExpression(init, st);
            const tLet = traceLet(name, init);
            return [[tLet, s], st1.set(name, false)];
        }
    }
}

/**
 * ```
 * t.traceWhile(identifier('c'));
 * while(c) {
 *  t.traceLoop();
 *  ...
 * }
 * t.exitBlock();
 * ```
 */
function reifyWhileStatement(s: t.WhileStatement, st: State): [t.Statement[], State] {
    const [test, st1] = reifyExpression(s.test, st);
    let [body, st2] = reifyStatement(s.body, st);
    body.unshift(traceLoop);
    const tWhile = traceWhile(test);
    const theWhile = t.whileStatement(s.test, t.blockStatement(body));
    return [[tWhile, theWhile, exitBlock], merge(st1, st2)];
}

/**
 * 
 * ```
 * let $test = identifier(c);
 * if(c) {
 *  t.traceIfTrue($test);
 *  ...
 * } else {
 *  t.traceIfFalse($test);
 * }
 * t.exitBlock();
 * ```
 * 
 */
function reifyIfStatement(s: t.IfStatement, st: State): [t.Statement[], State] {
    const [test, st1] = reifyExpression(s.test, st);
    let [ifTrue, st2] = reifyStatement(s.consequent, st);
    let [ifFalse, st3]: [t.Statement[], State] = [[], st];
    if(s.alternate !== null) {
        [ifFalse, st3] = reifyStatement(s.alternate, st);
    }
    const id = '$test';
    ifTrue.unshift(traceIfTrue(id));
    ifFalse.unshift(traceIfFalse(id));
    const tTest = jsLet(t.identifier(id), test);
    const theIf = t.ifStatement(s.test, t.blockStatement(ifTrue), t.blockStatement(ifFalse));
    return [[tTest, theIf, exitBlock], merge(merge(st1, st2), st3)];
}

function reifyExpressionStatement(s: t.ExpressionStatement, st: State): [t.Statement[], State] {
    const [expression, st1] = reifyExpression(s.expression, st);
    const above = t.expressionStatement(expression);
    return [[above, s], st1];
}

/**
 * 
 * ```
 * t.traceLabel('l');
 * l: {
 *  ...
 *  t.traceBreak('l');
 *  break l;
 *  t.exitBlock();
 * }
 * ```
 */
function reifyLabeledStatement(s: t.LabeledStatement, st: State): [t.Statement[], State] {
    const name = s.label;
    let [body, st1] = reifyStatement(s.body, st);
    body.push(exitBlock);
    const tLabel = traceLabel(lvaltoName(name));
    const theLabel = t.labeledStatement(name, t.blockStatement(body));
    return [[tLabel, theLabel], st1];
}

/**
 * ```
 * t.traceBreak('l');
 * break l;
 * ```
 */
function reifyBreakStatement(s: t.BreakStatement, st: State): [t.Statement[], State] {
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
 * t.traceLet('a', number(1));
 * let a = 1;
 * t.traceLet('F', clos({ a: identifier('a') }));
 * function F(x) {
 *  let [$clos, $x] = t.traceFunctionBody('$return');
 *  t.traceLet('x', $x);
 *  t.traceBreak('$return', binop('+', from($clos, 'a'), identifier('x')));
 *  return a + x; * 
 *  t.exitBlock();
 * }
 * ```
 */
function reifyFunctionDeclaration(s: t.FunctionDeclaration, st: State): [t.Statement[], State] {
    const id = s.id;
    if(id === null) {
        throw new Error("Null id!!");
    }
    const params = s.params;
    let funBodyLHS = [t.identifier('$clos')];
    let paramsBody: t.ExpressionStatement[] = [];
    let nextSt: State = Map();
    for(let i=0; i<params.length; i++) {
        const p = params[i];
        if(!t.isIdentifier(p)) {
            throw new Error("Expected identitifier!");
        } else {
            const oldName = lvaltoName(p);
            const newName = t.identifier('$' + oldName);
            funBodyLHS.push(newName);
            paramsBody.unshift(traceLet(oldName, newName));
            nextSt = nextSt.set(oldName, false);
        }
    }
    let [body, myState] = reifyStatement(s.body, nextSt);
    body.concat(paramsBody);
    body.unshift(jsLet(t.arrayPattern(funBodyLHS), traceFunctionBody()));
    body.push(exitBlock); // exit the label
    const retSt = st.set(lvaltoName(id), false);
    let fvs: t.ObjectProperty[] = [];
    myState.filter(v => v!)
        .keySeq().forEach(k => {
            if(retSt.has(k!)) {
                if(retSt.get(k!)!) {
                    fvs.push(t.objectProperty(t.identifier(k!), from(t.identifier('$clos'), k!)));
                } else {
                    fvs.push(t.objectProperty(t.identifier(k!), identifier(k!)));
                }
            } else {
                throw new Error("Not found!");
            }
        });
    const tClos = traceLet(lvaltoName(id), clos(fvs));
    const theFunction = t.functionDeclaration(id, params, t.blockStatement(body));
    return [[tClos, theFunction], retSt];
}

/**
 * ```
 * t.traceBreak('$return', number(42));
 * return 42;
 * ```
 */
function reifyReturnStatement(s: t.ReturnStatement, st: State): [t.Statement[], State] {
    if(s.argument === null) {
        throw new Error("Found null argument!");
    }
    const [argument, st1] = reifyExpression(s.argument, st);
    const tBreak = traceBreak(functionBreakName, argument);
    return [[tBreak, s], st1];
}

function reifyStatement(s: t.Statement, st: State): [t.Statement[], State] {
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
        default: return [[t.expressionStatement(t.stringLiteral('TODO: ' + s.type))], st];
    }
}

function reifyStatements(s: t.Statement[], st: State): [t.Statement[], State] {
    let ret: t.Statement[] = [];
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

function reify(s: t.Statement[]): t.Statement[] {
    const [ret, _] = reifyStatements(s, Map());
    ret.unshift(jsLet(t.identifier('t'), newTrace));
    ret.push(exitBlock);
    return ret;
}

export function transform(inputCode: string) {
    let normalized = n.normalize(inputCode);
    let ast = parser.parse(normalized);

    ast.program.body = reify(ast.program.body);
    console.log(generator(ast.program).code);
}

/**
 * Given an 'LVal' that is an identifier, produces the identifier's name.
 * Throws an exception if the 'LVal' is not an identifier.
 *
 * @param lval an l-value
 * @returns the name of the identifier, if 'lval' is an identifier
 */
function lvaltoName(lval: t.LVal): string {
    if (t.isIdentifier(lval)) {
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