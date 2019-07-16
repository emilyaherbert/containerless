import * as t from '@babel/types';

function traceLet(theArgs: t.Expression[]): t.ExpressionStatement {
    const memberExpression = t.memberExpression(t.identifier('t'), t.identifier('traceLet'));
    const callExpression = t.callExpression(memberExpression, theArgs);
    return t.expressionStatement(callExpression);
}

function reifyExpression(e: t.Expression): t.Expression {
    return e;
}

function reifyVariableDeclaration(s: t.VariableDeclaration): t.Statement[] {
    const id = s.declarations[0].id;
    const init = s.declarations[0].init;
    if(init === null) {
        throw new Error("Found null init.");
    } else {
        switch(init.type) {
            case 'CallExpression': {
                break;
            }
            default: {
                const theArgs = [t.identifier(lvaltoName(id)), reifyExpression(init)];
                const tLet = traceLet(theArgs);
                return [tLet, s];
            }
        }
    }

    return [t.expressionStatement(t.stringLiteral('TODO'))]
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