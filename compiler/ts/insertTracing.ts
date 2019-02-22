import * as babel from '@babel/core';
import * as t from '@babel/types';
import * as traverse from '@babel/traverse';
import * as template from '@babel/template';

function mkExpr(code: string) {
  const tmpl = template.expression(code,
      { placeholderPattern: /(EXPRESSION)/ });
  return function (binds: { [index: string]: any })  {
      let expr = tmpl(binds);
      (expr as any).__runtime__ = true;
      return expr;
  };
}

function mkExpr2(code: string) {
  const tmpl = template.expression(code,
      { placeholderPattern: /(EXPRESSION1)|(EXPRESSION2)/ });
  return function (binds: { [index: string]: any })  {
      let expr = tmpl(binds);
      (expr as any).__runtime__ = true;
      return expr;
  };
}

// Converts string into a statement that can be avoided by visitors
// by checking the __doNotVisit__ property.
function mkStmt(code: string) {
    const tmpl = template.statement(code,
        { placeholderPattern: /(VARIABLE)|(EXPRESSION)/ });
    return function (binds: { [index: string]: any })  {
        let stmt = tmpl(binds);
        (stmt as any).__runtime__ = true;
        return stmt;
    };
}

const buildNumericExpr = mkExpr(`$T.num(EXPRESSION)`);
const buildBooleanExpr = mkExpr(`$T.bool(EXPRESSION)`);
const buildStringExpr = mkExpr(`$T.str(EXPRESSION)`);
type EXPRMAP =  { [key: string]: (binds: { [index: string]: any; }) => t.Expression };
const buildUnaryExpr : EXPRMAP = ['neg','plus','not','bitnot']
    .reduce((ret : EXPRMAP, elem) => {
        ret[elem] = mkExpr(`$T.` + elem + `(EXPRESSION)`);
        return ret;
    }, {});
const buildLogicalExpr : EXPRMAP = ['and','or']
    .reduce((ret : EXPRMAP, elem) => {
        ret[elem] = mkExpr2(`$T.` + elem + `(EXPRESSION1, EXPRESSION2)`);
        return ret;
    }, {});
const buildBinaryExpr : EXPRMAP = ['add', 'gt', 'lt', 'geq', 'leq', 'sub', 'div', 'mul', 'remainder', 'pow', 'bitand', 'bitor', 'bitxor', 'rshift', 'unsignedrs', 'lshift', 'lshift', 'eq','ineq', 'exacteq','exactineq']
    .reduce((ret : EXPRMAP, elem) => {
        ret[elem] = mkExpr2(`$T.` + elem + `(EXPRESSION1, EXPRESSION2)`);
        return ret;
    }, {});
const buildArgumentExpr = mkExpr(`$T.argument(EXPRESSION)`);

const buildVarDeclaration = mkStmt(`let VARIABLE = $T.bind(EXPRESSION);`);
const buildVarUpdate = mkStmt(`$T.update(VARIABLE, EXPRESSION);`);
const buildInputDeclaration = mkStmt(`let VARIABLE = $T.input();`);
const buildParameterDeclaration = mkStmt(`let VARIABLE = $T.parameter();`);
const buildExpectReturnDeclaration = mkStmt(`let VARIABLE = $T.expectReturn();`)

const buildRequireStmt = mkStmt(`let VARIABLE = require('../dist/runtime');`);
const buildIfStmt = mkStmt(`$T.if_(EXPRESSION);`);
const buildIfElseStmt = mkStmt(`$T.ifElse(EXPRESSION);`);
const buildEnterIf = mkStmt(`$T.enterIf(EXPRESSION);`);
const buildReturnStmt = mkStmt(`$T.return_(EXPRESSION);`);
const buildArgumentStmt = mkStmt(`$T.argument(EXPRESSION);`);

type S = {
    names: Map<string, string>;
    stack: Map<string, string>[];
};

function st_clear(st: S): S {
  st.names = new Map([]);
  st.stack = [];
  return st;
}

function st_set(st: S, k: string, v: string): S {
  st.names.set(k, v);
  return st;
}

function st_get(st: S, k: string): string | undefined {
  if(st.names.has(k)) {
    return st.names.get(k);
  }
  for(let i=st.stack.length-1; i>(-1); i--) {
    if(st.stack[i].has(k)) {
      return st.stack[i].get(k);
    }
  }
}

function st_push(st: S): S {
  st.stack.push(new Map(st.names));
  st.names = new Map([]);
  return st;
}

function st_pop(st: S): S {
  if(st.stack.length > 0) {
    st.names = st.stack[st.stack.length - 1];
    st.stack.pop();
    return st;
  } else {
    throw "Found unexpected empty st.stack in st_pop."
  }
}

const visitor: traverse.Visitor<S> = {
    Program: {
        enter(path, st: S) {
            st_clear(st);
        },
        exit(path: traverse.NodePath<t.Program>, st) {
            const id = t.identifier('$T'); // TODO(arjun): capture

            let requireStmt = buildRequireStmt({
                VARIABLE: id
            });
            path.node.body.unshift(requireStmt);
        }
    },
    FunctionDeclaration: {
        enter(path: traverse.NodePath<t.FunctionDeclaration>, st) {
            if ((path.node as any).__runtime__) {
              path.skip();
              return;
            }
            const params = path.node.params;
            for (let i = 0; i < params.length; ++i) {
                const param = params[i];
                if (param.type !== 'Identifier') {
                    throw new Error('only identifier params supported');
                }
                const newParam = path.scope.generateUidIdentifierBasedOnNode(param);
                st_set(st, param.name, newParam.name);
            }
        },
        exit(path: traverse.NodePath<t.FunctionDeclaration>, st) {
            const name = path.node.id;
            const body = path.node.body.body;
            const params = path.node.params;
            // TODO(emily): Change hard-coded main.
            if(name === null) {
              throw "Function name cannot be null.";
              return;
            }
            if(name.name === 'main') {
                for (let i = 0; i < params.length; ++i) {
                    const param = params[i];
                    if (param.type !== 'Identifier') {
                        throw new Error('only identifier params supported');
                    }
                    const inputDeclaration = buildInputDeclaration({
                        VARIABLE: st_get(st, param.name)
                    });
                    body.unshift(inputDeclaration);
                }
            } else {
                for (let i = 0; i < params.length; ++i) {
                    const param = params[i];
                    if (param.type !== 'Identifier') {
                        throw new Error('only identifier params supported');
                    }
                    const parameterDeclaration = buildParameterDeclaration({
                      VARIABLE: st_get(st, param.name)
                    })
                    body.unshift(parameterDeclaration);
                }
            }
        }
    },
    IfStatement: {
        enter(path: traverse.NodePath<t.IfStatement>, st) {
            if ((path.node as any).__runtime__) {
              path.skip();
              return;
            }

            const innerExpr = path.node.test;
            const consequent = path.node.consequent;
            const alternate = path.node.alternate;

            if(alternate === null) {
              // If statement.

              const ifStmt = buildIfStmt({
                EXPRESSION: reifyExpr(st, innerExpr)
              })
              path.insertBefore(ifStmt);

              if (!t.isBlockStatement(consequent)) {
                path.node.consequent = t.blockStatement([consequent]);
              }
              const enterIfTrue = buildEnterIf({
                EXPRESSION: t.booleanLiteral(true)
              });
              (path.node.consequent as t.BlockStatement).body.unshift(enterIfTrue);

            } else {
              // IfElse statement.

              const ifElseStmt = buildIfElseStmt({
                EXPRESSION: reifyExpr(st, innerExpr)
              })
              path.insertBefore(ifElseStmt);

              if (!t.isBlockStatement(consequent)) {
                path.node.consequent = t.blockStatement([consequent]);
              }
              const enterIfTrue = buildEnterIf({
                EXPRESSION: t.booleanLiteral(true)
              });
              (path.node.consequent as t.BlockStatement).body.unshift(enterIfTrue);
              
              if (!t.isBlockStatement(alternate)) {
                  path.node.alternate = t.blockStatement([alternate]);
              }
              const enterIfFalse = buildEnterIf({
                EXPRESSION: t.booleanLiteral(false)
              });
              (path.node.alternate as t.BlockStatement).body.unshift(enterIfFalse);
            }
        }
    },
    VariableDeclaration: {
        enter(path: traverse.NodePath<t.VariableDeclaration>, st) {
            if ((path.node as any).__runtime__) {
                path.skip();
                return;
            }
            if (path.node.declarations.length !== 1) {
              throw new Error('expected exactly one declaration');
            }
            const declaration = path.node.declarations[0];
            const id = declaration.id;
            const newId = path.scope.generateUidIdentifierBasedOnNode(id);
            st_set(st, lvaltoName(id), newId.name);
            const expr = declaration.init || eUndefined;
            
            if(t.isCallExpression(expr)) {
              const args = expr.arguments;
              for(let i=0; i<args.length; i++) {
                const arg = args[i];
                if(arg.type !== 'Identifier') {
                  throw new Error('Only Identifier args supported.');
                }
                const argumentStmt = buildArgumentStmt({
                  EXPRESSION: reifyExpr(st, arg)
                })
                path.insertBefore(argumentStmt);
              }
              const expectReturnDeclaration = buildExpectReturnDeclaration({
                VARIABLE: newId,
              })
              path.insertAfter(expectReturnDeclaration);
            } else {
              const varDeclaration = buildVarDeclaration({
                  VARIABLE: newId,
                  EXPRESSION: reifyExpr(st, expr)
              });
              path.insertBefore(varDeclaration);
            }
        }
    },
    // CallExpression: {
    //     enter(path: traverse.NodePath<t.CallExpression>, st) {
    //         if ((path.node as any).__runtime__) {
    //           path.skip();
    //           return;
    //         }
    //         const args = path.node.arguments;
    //         for(let i=0; i<args.length; i++) {
    //             const arg = args[i];
    //             if (arg.type !== 'Identifier') {
    //                 throw new Error('Only Identifier args supported.');
    //             }
    //             const argumentStmt = buildArgumentStmt({
    //               EXPRESSION: reifyExpr(st, arg)
    //             })
    //             path.insertBefore(argumentStmt);
    //         }
    //     }
    // },
    ExpressionStatement: {
        enter(path: traverse.NodePath<t.ExpressionStatement>, st) {
            if ((path.node as any).__runtime__) {
              path.skip();
              return;
            }
            if (t.isAssignmentExpression(path.node.expression)) {
                if (path.node.expression.operator === '=') {
                    const origExpr = path.node.expression;
                    const rightExpr = origExpr.right;
                    const varName = st_get(st, lvaltoName(origExpr.left));
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
    BlockStatement: {
        enter(path: traverse.NodePath<t.BlockStatement>, st) {
            if ((path.node as any).__runtime__) {
              path.skip();
              return;
            }
            st_push(st);
        },
        exit(path: traverse.NodePath<t.BlockStatement>, st) {
            st_pop(st);
        }
    },
    ReturnStatement: {
        enter(path: traverse.NodePath<t.ReturnStatement>, st) {
            if ((path.node as any).__runtime__) {
              path.skip();
              return;
            }
            let innerExpr = path.node.argument;
            if (innerExpr === null) {
                innerExpr = eUndefined;
            }
            const returnStmt = buildReturnStmt({
              EXPRESSION: reifyExpr(st, innerExpr)
            })
            path.insertBefore(returnStmt);
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
        const numericExpr = buildNumericExpr({
          EXPRESSION: expr
        });
        return numericExpr;
    }
    else if (expr.type === 'BooleanLiteral') {
        const booleanExpr = buildBooleanExpr({
          EXPRESSION: expr
        })
        return booleanExpr;
    }
    else if (expr.type === 'Identifier') {
        let x = st_get(st, expr.name);
        if (x === undefined) {
            console.log(expr);
            throw new Error(`no binding for ${expr.name}`);
        }
        return t.identifier(x);
    }
    else if (expr.type === 'CallExpression') {
      return t.sequenceExpression(expr.arguments.map(e =>
        buildArgumentExpr({ EXPRESSION: reifyExpr(st, e as t.Expression) })));
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
        const unaryExpr = buildUnaryExpr[traceOpName]({
          EXPRESSION: e
        })
        return unaryExpr;
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
      const logicalExpr = buildLogicalExpr[traceOpName]({
        EXPRESSION1: e1,
        EXPRESSION2: e2
      })
      return logicalExpr;
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
        const binaryExpr = buildBinaryExpr[traceOpName]({
          EXPRESSION1: e1,
          EXPRESSION2: e2
        })
        return binaryExpr;
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
