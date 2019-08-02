import * as t from '@babel/types';

export type NormalizedVariableDeclarator = {
    init: t.Expression
} & t.VariableDeclarator;

export type NormalizedVariableDeclaration = {
    declarations: [ NormalizedVariableDeclarator ]
} & t.VariableDeclaration;

export type NormalizedCallExpression = {
    callee: t.Identifier | t.MemberExpression,
    arguments: Array<t.Expression>
} & t.CallExpression;

export type NormalizedMemberExpression = {
    object: t.Identifier,
    property: t.Identifier
} & t.MemberExpression;

/**
 * When the type of 'node' is statically known, we use a conditional type to
 * narrow it to what Stopify ensures. In addition, at runtime, we check to see
 * that these assumptions are correct.
 *
 * https://www.typescriptlang.org/docs/handbook/advanced-types.html#conditional-types
 */
export function assertNormalized<T extends t.Node>(node: T):
  T extends t.VariableDeclaration ? NormalizedVariableDeclaration :
  T extends t.CallExpression ? NormalizedCallExpression : 
  T extends t.MemberExpression ? NormalizedMemberExpression : unknown {
    if (t.isVariableDeclaration(node)) {
        if (node.declarations.length !== 1) {
            throw new Error('expected exactly one declaration');
        }
        if (node.declarations[0].init === null) {
            throw new Error('expected initialized variable');
        }
        // Unfortunately, TypeScript's type checker cannot ensure that this is
        // well-typed. But, we know what we are doing, so just cast to any.
        return node as any;
    }
    else if (t.isCallExpression(node)) {
        if (!t.isIdentifier(node.callee) && !t.isMemberExpression(node.callee)) {
            throw new Error('expected identifier or member expression in function position');
        }
        if (!node.arguments.every(arg => t.isExpression(arg))) {
            throw new Error('all arguments must be expressions');
        }
        return node as any;
    } else if(t.isMemberExpression(node)) {
        if(!t.isIdentifier(node.property)) {
            throw new Error("Cannot chain member expressions!");
        }
        if(!t.isIdentifier(node.object)) {
            throw new Error("Cannot chain member expressions!");
        }
        return node as any;
    }
    else {
        throw new Error(`Cannot normalize ${node.type}`);
    }

}