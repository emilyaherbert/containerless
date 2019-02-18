import * as babel from '@babel/core';
import * as t from '@babel/types';
import * as traverse from '@babel/traverse';
import * as template from '@babel/template';

// Converts string into a statement that can be avoided by visitors
// by checking the __doNotVisit__ property.
function mkStmt(code: string) {
    const tmpl = template.statement(code,
        { placeholderPattern: /(VARIABLE)|(EXPRESSION)/ });
    return function (binds: { [index: string]: any })  {
        let stmt = tmpl(binds);
        (stmt as any).__doNotVisit__ = true;
        return stmt;
    };
}

const buildVarDeclaration = mkStmt(`let VARIABLE = $T.bind(EXPRESSION);`);
const buildVarUpdate = mkStmt(`$T.update(VARIABLE, EXPRESSION);`);
const buildLogicalExpression = template.statement(`LEXPR OP REXPR;`);

type S = {
    names: Map<string, string>
};

const visitor: traverse.Visitor<S> = {
    Program: {
        enter(path, st: S) {
            st.names = new Map();
        },
        exit(path: traverse.NodePath<t.Program>, st) {
            const id = t.identifier('$T'); // TODO(arjun): capture

            let buildRequireStmt = template.statement(`let VARIABLE = require('../dist/runtime');`);
            let requireStmt = buildRequireStmt({
                VARIABLE: id
            });
            path.node.body.unshift(requireStmt);
        }
    },
    FunctionDeclaration: {
        enter(path: traverse.NodePath<t.FunctionDeclaration>, st) {
            const body = path.node.body.body;
            for (let i = 0; i < path.node.params.length; ++i) {
                const param = path.node.params[i];
                if (param.type !== 'Identifier') {
                    throw new Error('only identifier params supported');
                }
                const newParam = path.scope.generateUidIdentifierBasedOnNode(param);
                st.names.set(param.name, newParam.name);
            }
        },
        exit(path: traverse.NodePath<t.FunctionDeclaration>, st) {
            const body = path.node.body.body;
            for (let i = 0; i < path.node.params.length; ++i) {
                const param = path.node.params[i];
                if (param.type !== 'Identifier') {
                    throw new Error('only identifier params supported');
                }
                const newParam = st.names.get(param.name);
                const buildInputDeclaration = template.statement(
                    `let VARIABLE = $T.input();`,
                    { placeholderPattern: /VARIABLE/ }); // avoid $T being captured
                const inputDeclaration = buildInputDeclaration({
                    VARIABLE: newParam
                });
                body.unshift(inputDeclaration);
            }
        }
    },
    IfStatement: {
        enter(path: traverse.NodePath<t.IfStatement>, st) {
            const innerExpr = path.node.test;
            path.insertBefore(call(mem(t.identifier('$T'), 'if_'), reifyExpr(st, innerExpr)));
            const consequent = path.node.consequent;
            // if the consequent isn't a block statement, then just shove it into one
            if (!t.isBlockStatement(consequent)) {
                const newBlockStmt = t.blockStatement([consequent]);
                path.node.consequent = newBlockStmt;
            }
            (path.node.consequent as t.BlockStatement).body.unshift(t.expressionStatement(
                call(mem(t.identifier('$T'), 'enterIf'), t.booleanLiteral(true))));
            const alternate = path.node.alternate;
            if (alternate !== null) {
                // if the alternate isn't a block statement, then just stuff it inside one
                if (!t.isBlockStatement(alternate)) {
                    const newBlockStmt = t.blockStatement([alternate]);
                    path.node.alternate = newBlockStmt;
                }
                (path.node.alternate as t.BlockStatement).body.unshift(t.expressionStatement(
                    call(mem(t.identifier('$T'), 'enterIf'), t.booleanLiteral(false))));
            }
        }
    },
    VariableDeclaration: {
        enter(path: traverse.NodePath<t.VariableDeclaration>, st) {
            if ((path.node as any).__doNotVisit__) {
                path.skip();
                return;
            }
            const declarations = path.node.declarations;
            for (let i = 0; i < declarations.length; ++i) {
                const id = declarations[i].id;
                const newId = path.scope.generateUidIdentifierBasedOnNode(id);
                st.names.set(lvaltoName(id), newId.name);
                const expr = declarations[i].init || eUndefined;
                const varDeclaration = buildVarDeclaration({
                    VARIABLE: newId,
                    EXPRESSION: reifyExpr(st, expr)
                });
                path.insertBefore(varDeclaration);
            }
        }
    },
    ExpressionStatement: {
        enter(path: traverse.NodePath<t.ExpressionStatement>, st) {
            if (t.isAssignmentExpression(path.node.expression)) {
                if (path.node.expression.operator === '=') {
                    const origExpr = path.node.expression;
                    const rightExpr = origExpr.right;
                    const varName = st.names.get(lvaltoName(origExpr.left));
                    if (varName === undefined) {
                        throw new Error('assigning to an undeclared variable');
                    }
                    const varUpdate = buildVarUpdate({
                        VARIABLE: varName,
                        EXPRESSION: reifyExpr(st, rightExpr)
                    });
                    path.insertBefore(varUpdate);
                }
            }
        }
    },
    ReturnStatement: {
        enter(path: traverse.NodePath<t.ReturnStatement>, st) {
            let innerExpr = path.node.argument;
            if (innerExpr === null) {
                innerExpr = eUndefined;
            }
            path.insertBefore(call(mem(t.identifier('$T'), 'return_'), reifyExpr(st, innerExpr)));
        }
    }
};

let eUndefined = t.unaryExpression('void', t.numericLiteral(0));

function call(f: t.Expression, ...args: t.Expression[]): t.Expression {
    return t.callExpression(f, args);
}

function mem(object: t.Expression, field: string) {
    return t.memberExpression(object, t.identifier(field));
}

// TODO(emily): Just want to clarify - why have we done this with a seperate function and not with babel? 
function reifyExpr(st: S, expr: t.Expression): t.Expression {
    if (expr.type === 'NumericLiteral') {
        return call(mem(t.identifier('$T'), 'num'), expr);
    }
    else if (expr.type === 'BooleanLiteral') {
        return call(mem(t.identifier('$T'), 'bool'), expr);
    }
    else if (expr.type === 'Identifier') {
        let x = st.names.get(expr.name);
        if (x === undefined) {
            throw new Error(`no binding for ${x}`);
        }
        return t.identifier(x);
    }
    else if (expr.type === 'UnaryExpression') {
        let e = reifyExpr(st, expr.argument);
        let traceOpName: string;
        switch (expr.operator) {
            case '-': traceOpName = 'neg'; break;
            case '+': traceOpName = 'plus'; break;
            case '!': traceOpName = 'not'; break;
            case '~': traceOpName = 'bitnot'; break;
            default: // cases 'typeof', 'void', 'delete', 'throw' not handled
                throw new Error(`unsupported binary operator: ${expr.operator}`);
        }
        return call(mem(t.identifier('$T'), traceOpName), e);
    }
    else if (expr.type == 'LogicalExpression') {
      let e1 = reifyExpr(st, expr.left);
      let e2 = reifyExpr(st, expr.right);
      let traceOpName: string;
      switch (expr.operator) {
        case '&&':
          traceOpName = 'and';
          break;
        case '||':
          traceOpName = 'or';
          break;
        default:
          throw new Error(`unsupported logical operator: ${expr.operator}`);
      }
      return call(mem(t.identifier('$T'), traceOpName), e1, e2);
    }
    else if (expr.type === 'BinaryExpression') {
        // TODO(Chris): Do we try to perform type checking here to see what the
        // left and right expressions are (to handle commonly used overloaded
        // ops like '+')? Our current implementation of $T suggests it will
        // have separate adding functions, +num and +str.
        let e1 = reifyExpr(st, expr.left);
        let e2 = reifyExpr(st, expr.right);
        let traceOpName: string;
        switch (expr.operator) {
          case '+':   traceOpName = 'add'; break;
          case '>':   traceOpName = 'gt'; break;
          case '<':   traceOpName = 'lt'; break;
          case '>=':  traceOpName = 'geq'; break;
          case '<=':  traceOpName = 'leq'; break;
          case '-':   traceOpName = 'sub'; break;
          case '/':   traceOpName = 'div'; break;
          case '*':   traceOpName = 'mul'; break;
          case '%':   traceOpName = 'remainder'; break;
          case '**':  traceOpName = 'pow'; break;
          case '&':   traceOpName = 'bitand'; break;
          case '|':   traceOpName = 'bitor'; break;
          case '^':   traceOpName = 'bitxor'; break;
          case '>>':  traceOpName = 'rshift'; break;
          case '>>>': traceOpName = 'unsignedrshift'; break;
          case '<<':  traceOpName = 'lshift'; break;
          case '<<':  traceOpName = 'lshift'; break;
          case '==':  traceOpName = 'eq'; break;
          case '!=':  traceOpName = 'ineq'; break;
          case '===': traceOpName = 'exacteq'; break;
          case '!==': traceOpName = 'exactineq'; break;
          default: // cases 'in', and 'instanceof' not handled
              throw new Error(`unsupported binary operator: ${expr.operator}`);
        }
        return call(mem(t.identifier('$T'), traceOpName), e1, e2);
    }
    else {
        throw new Error(`unsupported expression type: ${expr.type}`);
    }
}

/**
 * Given an 'LVal' that is an identifier, produces the identifier's name.
 * Throws an exception if the 'LVal' is not an identifier.
 *
 * @param lval an l-value
 * @returns the name of the identifier, if 'lval' is an identifier
 */
function lvaltoName(lval: t.LVal): string {
    if (lval.type === 'Identifier') {
        return lval.name;
    } else if (lval.type === 'RestElement' && lval.argument.type === 'Identifier') {
        return lval.argument.name;
    } else {
        throw new Error(`Expected Identifier, received ${lval.type}`);
    }
}

export function plugin() {
    return { visitor: visitor };
}

export function transform(code: string) {
    const result = babel.transformSync(code, {
        babelrc: false,
        plugins: [ plugin ],
        ast: false,
        code: true
    });
    if (result === null) {
        throw new Error('no result');
    }
    if (typeof result.code !== 'string') {
        throw new Error('no code');
    }
    return result.code;
}
