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

export type NormalizedReturnStatement = {
    argument: t.Expression
} & t.ReturnStatement;

export type NormalizedObjectProperty = {
    key: t.Identifier,
    value: t.Expression
} & t.ObjectProperty;

export type NormalizedObjectExpression = {
    properties: NormalizedObjectProperty[]
} & t.ObjectExpression;

export type NormalizedAssignmentExpression = {
    left: t.Identifier | t.MemberExpression,
    right: t.Expression
} & t.AssignmentExpression;

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
  T extends t.MemberExpression ? NormalizedMemberExpression :
  T extends t.ObjectProperty ? NormalizedObjectProperty :
  T extends t.ObjectExpression ? NormalizedObjectExpression :
  T extends t.AssignmentExpression ? NormalizedAssignmentExpression :
  T extends t.ReturnStatement ? NormalizedReturnStatement : unknown {
    if (t.isVariableDeclaration(node)) {
        if (node.declarations.length !== 1) {
            throw new Error('expected exactly one declaration');
        }
        if (node.declarations[0].init === null) {
            console.log(node.declarations[0]);
            console.log(node.declarations[0].id);
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
    else if (t.isReturnStatement(node)) {
        if(node.argument === null) {
            // NOTE(arjun): I think it is a little hacky that assertNormalized
            // also performs some normalization.
            node.argument = t.unaryExpression('void', t.numericLiteral(0));
        }
        return node as any;
    }
    else if (t.isObjectProperty(node)) {
        if(!t.isIdentifier(node.key) && !t.isStringLiteral(node.key)) {
            throw new Error("Found key that is not an identifier or string.")
        }
        if(!t.isExpression(node.value)) {
            throw new Error("Found value that is not an expression.");
        }
        return node as any;
    }
    else if (t.isObjectExpression(node)) {
        const properties = node.properties;
        for(let i=0; i<properties.length; i++) {
            assertNormalized(properties[i]);
        }
        return node as any;
    }
    else if (t.isAssignmentExpression(node)) {
        if(!t.isIdentifier(node.left) && !t.isMemberExpression(node.left) || !t.isExpression(node.right)) {
            throw new Error("Found something wrong.");
        }
        return node as any;
    }
    else {
        throw new Error(`Cannot normalize ${node.type}`);
    }

}