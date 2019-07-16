import * as t from '@babel/types';

function identifier(s: string): t.Expression {
    const callee = t.identifier('identifier');
    const theArgs = [t.stringLiteral(s)];
    return t.callExpression(callee, theArgs);
}

function number(n: number): t.Expression {
    const callee = t.identifier('number');
    const theArgs = [t.numericLiteral(n)];
    return t.callExpression(callee, theArgs);
}

function boolean(b: boolean): t.Expression {
    const callee = t.identifier('boolean');
    const theArgs = [t.booleanLiteral(b)];
    return t.callExpression(callee, theArgs);
}

function traceLet(theArgs: t.Expression[]): t.ExpressionStatement {
    const memberExpression = t.memberExpression(t.identifier('t'), t.identifier('traceLet'));
    const callExpression = t.callExpression(memberExpression, theArgs);
    return t.expressionStatement(callExpression);
}

// TODO(emily): Change this to LVal later.
function traceFunctionCall(id: string, theArgs: t.Expression[]): t.ExpressionStatement {
    const memberExpression = t.memberExpression(t.identifier('t'), t.identifier('traceFunctionCall'));
    const memberArgs = [t.stringLiteral(id), t.arrayExpression(theArgs)];
    const callExpression = t.callExpression(memberExpression, memberArgs);
    return t.expressionStatement(callExpression);
}

function exitBlock(): t.ExpressionStatement {
    const memberExpression = t.memberExpression(t.identifier('t'), t.identifier('exitBlock'));
    const callExpression = t.callExpression(memberExpression, []);
    return t.expressionStatement(callExpression);
}

function reifyExpression(e: t.Expression): t.Expression {
    switch(e.type) {
        case 'NumericLiteral': return number(e.value);
        case 'BooleanLiteral': return boolean(e.value);
        default: throw new Error("Type not implemented.")
    }
}

function reifyVariableDeclaration(s: t.VariableDeclaration): t.Statement[] {
    const id = s.declarations[0].id;
    const init = s.declarations[0].init;
    if(init === null) {
        throw new Error("Found null init.");
    } else {
        switch(init.type) {
            case 'CallExpression': {
                if(!t.isIdentifier(init.callee)) {
                    throw new Error("Expected id callee.");
                }
                const callee = init.callee.name;
                let theArgs: t.Expression[] = [identifier(callee)];
                init.arguments.forEach(a => {
                    if(t.isSpreadElement(a)) {
                        throw new Error("Found spread element.");
                    } else if(t.isJSXNamespacedName(a)) {
                        throw new Error("Found JSXNamespacedName.");
                    } else if(t.isArgumentPlaceholder(a)) {
                        throw new Error("Found argument placeholder.");
                    }
                    theArgs.push(reifyExpression(a));
                })
                const tCall = traceFunctionCall(lvaltoName(id), theArgs);
                const tExit = exitBlock();
                return [tCall, s, tExit];
            }
            default: {
                const tLet = traceLet([t.identifier(lvaltoName(id)), reifyExpression(init)]);
                return [tLet, s];
            }
        }
    }
}

function reifyStatement(s: t.Statement): t.Statement[] {
    switch(s.type) {
        case 'VariableDeclaration': return reifyVariableDeclaration(s);
        default: console.log(s.type);
    }



    return [t.expressionStatement(t.stringLiteral('TODO'))]
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