import * as t from '@babel/types';
import { assertNormalized } from './assertNormalized';

function identifier(s: string): t.CallExpression {
    const callee = t.identifier('identifier');
    const theArgs = [t.stringLiteral(s)];
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

const undefined_: t.Identifier = t.identifier('undefined_');

function binop(op: string, e1: t.Expression, e2: t.Expression): t.CallExpression {
    const callee = t.identifier('binop');
    const theArgs = [t.stringLiteral(op), e1, e2];
    return t.callExpression(callee, theArgs);
}

function traceLet(theArgs: t.Expression[]): t.ExpressionStatement {
    const memberExpression = t.memberExpression(t.identifier('t'), t.identifier('traceLet'));
    const callExpression = t.callExpression(memberExpression, theArgs);
    return t.expressionStatement(callExpression);
}

function jsLet(id: string, rhs: t.Expression): t.VariableDeclaration {
    const variableDeclarator = t.variableDeclarator(t.identifier(id), rhs);
    return t.variableDeclaration('let', [ variableDeclarator ]);
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

function traceLoop(): t.ExpressionStatement {
    const memberExpression = t.memberExpression(t.identifier('t'), t.identifier('traceLoop'));
    const callExpression = t.callExpression(memberExpression, []);
    return t.expressionStatement(callExpression);
}

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

function traceLabel(name: string): t.ExpressionStatement {
    const memberExpression = t.memberExpression(t.identifier('t'), t.identifier('traceLabel'));
    const callExpression = t.callExpression(memberExpression, [t.stringLiteral(name)]);
    return t.expressionStatement(callExpression);
}

function traceBreak(name: string, value = undefined_): t.ExpressionStatement {
    const memberExpression = t.memberExpression(t.identifier('t'), t.identifier('traceBreak'));
    const callExpression = t.callExpression(memberExpression, [t.stringLiteral(name), value]);
    return t.expressionStatement(callExpression);
}

function exitBlock(): t.ExpressionStatement {
    const memberExpression = t.memberExpression(t.identifier('t'), t.identifier('exitBlock'));
    const callExpression = t.callExpression(memberExpression, []);
    return t.expressionStatement(callExpression);
}

function reifyExpression(e: t.Expression): t.Expression {
    switch(e.type) {
        case 'Identifier': return identifier(e.name);
        case 'NumericLiteral': return number(e.value);
        case 'BooleanLiteral': return boolean(e.value);
        case 'BinaryExpression': return binop(e.operator, reifyExpression(e.left), reifyExpression(e.right));
        case 'AssignmentExpression': return traceSet(lvaltoName(e.left), reifyExpression(e.right));
        default: throw new Error(e.type + ' not implemented.');
    }
}

function reifyVariableDeclaration(s: t.VariableDeclaration): t.Statement[] {
    let s1 = assertNormalized(s);
    const id = s1.declarations[0].id;
    const init = s1.declarations[0].init;
    switch(init.type) {
        case 'CallExpression': {
            let init1 = assertNormalized(init);
            const callee = init1.callee.name;
            let theArgs: t.Expression[] = [identifier(callee)];
            init1.arguments.forEach(a => {
                theArgs.push(reifyExpression(a));
            });
            const tCall = traceFunctionCall(lvaltoName(id), theArgs);
            return [tCall, s, exitBlock()];
        }
        default: {
            const tLet = traceLet([t.identifier(lvaltoName(id)), reifyExpression(init)]);
            return [tLet, s];
        }
    }
}

function reifyWhileStatement(s: t.WhileStatement): t.Statement[] {
    const test = reifyExpression(s.test);
    let body = reifyStatement(s.body);
    body.unshift(traceLoop());
    const tWhile = traceWhile(test);
    const theWhile = t.whileStatement(s.test, t.blockStatement(body));
    return [tWhile, theWhile, exitBlock()];
}

function reifyIfStatement(s: t.IfStatement): t.Statement[] {
    const test = reifyExpression(s.test);
    let ifTrue = reifyStatement(s.consequent);
    let ifFalse: t.Statement[] = [];
    if(s.alternate !== null) {
        ifFalse = reifyStatement(s.alternate);
    }
    const id = '$test';
    ifTrue.unshift(traceIfTrue(id));
    ifFalse.unshift(traceIfFalse(id));
    const tTest = jsLet(id, test);
    const theIf = t.ifStatement(s.test, t.blockStatement(ifTrue), t.blockStatement(ifFalse));
    return [tTest, theIf, exitBlock()];
}

function reifyExpressionStatement(s: t.ExpressionStatement): t.Statement[] {
    const above = t.expressionStatement(reifyExpression(s.expression));
    return [above, s];
}

function reifyLabeledStatement(s: t.LabeledStatement): t.Statement[] {
    const name = s.label;
    let body = reifyStatement(s.body);
    body.push(exitBlock());
    const tLabel = traceLabel(lvaltoName(name));
    const theLabel = t.labeledStatement(name, t.blockStatement(body));
    return [tLabel, theLabel];
}

function reifyBreakStatement(s: t.BreakStatement): t.Statement[] {
    const name = s.label;
    if(name === null) {
        throw new Error("Found null label in break.");
    } else {
        const tBreak = traceBreak(lvaltoName(name));
        return [tBreak, s];
    }
}

function reifyStatement(s: t.Statement): t.Statement[] {
    switch(s.type) {
        case 'VariableDeclaration': return reifyVariableDeclaration(s);
        case 'WhileStatement': return reifyWhileStatement(s);
        case 'BlockStatement': return reifyStatements(s.body); // NOTE: this unwraps block statements.
        case 'IfStatement': return reifyIfStatement(s);
        case 'ExpressionStatement': return reifyExpressionStatement(s);
        case 'LabeledStatement': return reifyLabeledStatement(s);
        case 'BreakStatement': return reifyBreakStatement(s);
        default: return [t.expressionStatement(t.stringLiteral('TODO: ' + s.type))]
    }
}

export function reifyStatements(s: t.Statement[]): t.Statement[] {
    let ret: t.Statement[] = [];

    for(let i=0; i<s.length; i++) {
        let r = reifyStatement(s[i]);
        for(let j=0; j<r.length; j++) {
            ret.push(r[j]);
        }
    }

    return ret;
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