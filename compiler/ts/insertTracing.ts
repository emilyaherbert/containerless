import * as babel from '@babel/core';
import * as t from '@babel/types';
import * as traverse from '@babel/traverse';
import * as template from '@babel/template';

function mkExpr(code: string) {
  const tmpl = template.expression(code,
      { placeholderPattern: /(EXPRESSION\d?)/ });
  return function (binds: { [index: string]: any })  {
      let expr = tmpl(binds);
      (expr as any).__runtime__ = true;
      return expr;
  };
}

// Converts string into a statement that can be avoided by visitors
// by checking the __runtime__ property.
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
type EXPRMAP = { [key: string]: (binds: { [index: string]: any; }) => t.Expression };
const buildUnaryExpr : EXPRMAP = ['neg','plus','not','bitnot']
    .reduce((ret : EXPRMAP, elem) => {
        ret[elem] = mkExpr(`$T.` + elem + `(EXPRESSION)`);
        return ret;
    }, {});
const buildLogicalExpr : EXPRMAP = ['and','or']
    .reduce((ret : EXPRMAP, elem) => {
        ret[elem] = mkExpr(`$T.` + elem + `(EXPRESSION1, EXPRESSION2)`);
        return ret;
    }, {});
const buildBinaryExpr : EXPRMAP = ['add', 'gt', 'lt', 'geq', 'leq', 'sub', 'div', 'mul', 'remainder', 'pow', 'bitand', 'bitor', 'bitxor', 'rshift', 'unsignedrs', 'lshift', 'lshift', 'eq', 'ineq', 'exacteq', 'exactineq']
    .reduce((ret : EXPRMAP, elem) => {
        ret[elem] = mkExpr(`$T.` + elem + `(EXPRESSION1, EXPRESSION2)`);
        return ret;
    }, {});
const buildArgumentExpr = mkExpr(`$T.argument(EXPRESSION)`);
const buildTernaryExpr = mkExpr(`$T.ternary(EXPRESSION1, EXPRESSION2, EXPRESSION3)`);
const buildObjectExpr = mkExpr(`$T.object(EXPRESSION)`);
const buildMemberExpr = mkExpr(`$T.member(EXPRESSION1, EXPRESSION2)`);

const buildVarDeclaration = mkStmt(`let VARIABLE = $T.bind(EXPRESSION);`);
const buildVarUpdate = mkStmt(`$T.update(VARIABLE, EXPRESSION);`);
const buildInputDeclaration = mkStmt(`let VARIABLE = $T.input();`);
const buildExpectReturnDeclaration = mkStmt(`let VARIABLE = $T.expectReturn();`)

const buildStartTraceStmt = mkStmt(`$T.startTrace();`)
const buildStopTraceStmt = mkStmt(`$T.stopTrace();`);
const buildRequireStmt = mkStmt(`let VARIABLE = require('../dist/runtime');`);
const buildIfStmt = mkStmt(`$T.if_(EXPRESSION);`);
const buildIfElseStmt = mkStmt(`$T.ifElse(EXPRESSION);`);
const buildExitIfStmt = mkStmt(`$T.exitIf();`);
const buildEnterIf = mkStmt(`$T.enterIf(EXPRESSION);`);
const buildReturnStmt = mkStmt(`$T.return_(EXPRESSION);`);
const buildArgsStmt = mkStmt(`$T.args(EXPRESSION);`);
const buildParamsStmt = mkStmt(`let EXPRESSION = $T.params();`);

let eUndefined = t.unaryExpression('void', t.numericLiteral(0));

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
    st.names = st.stack.pop()!;
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
            const startTraceStmt = buildStartTraceStmt({});
            path.node.body.unshift(startTraceStmt);

            // TODO(emily): Find better way to insert $T.stopTrace() as
            // second to last element.
            const stopTraceStmt = buildStopTraceStmt({});
            path.node.body.splice(path.node.body.length - 1, 0, stopTraceStmt);

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
              throw new Error("Function name cannot be null.");
            }
            if(name.name === 'main') {
                for (let i = 0; i < params.length; ++i) {
                    const param = params[i];
                    if (!t.isIdentifier(param)) {
                        throw new Error('only identifier params supported');
                    }
                    const inputDeclaration = buildInputDeclaration({
                        VARIABLE: st_get(st, param.name)
                    });
                    body.unshift(inputDeclaration);
                }
            } else {
                const param_names : t.Identifier[] = [];
                for (let i = 0; i < params.length; ++i) {
                    const param = params[i];
                    if (!t.isIdentifier(param)) {
                        throw new Error('only identifier params supported');
                    }
                    const name = st_get(st, param.name);
                    if(name !== undefined) {
                      param_names[i] = t.identifier(name);
                    } else {
                      throw new Error("Found undefined name in FunctionDeclaration.");
                    }
                }
                const paramsStmt = buildParamsStmt({
                  EXPRESSION: t.arrayPattern(param_names)
                })
                body.unshift(paramsStmt);
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

            const exitIfStmt = buildExitIfStmt({});

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
              (path.node.consequent as t.BlockStatement).body.push(exitIfStmt);

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
              (path.node.consequent as t.BlockStatement).body.push(exitIfStmt);
              
              if (!t.isBlockStatement(alternate)) {
                  path.node.alternate = t.blockStatement([alternate]);
              }
              const enterIfFalse = buildEnterIf({
                EXPRESSION: t.booleanLiteral(false)
              });
              (path.node.alternate as t.BlockStatement).body.unshift(enterIfFalse);
              (path.node.alternate as t.BlockStatement).body.push(exitIfStmt);
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
              let exprs : t.Expression[] = [];
              for(let i=0; i<args.length; i++) {
                const arg = args[i];
                if(!t.isIdentifier(arg)) {
                  throw new Error('Only Identifier args supported.');
                }
                exprs.push(reifyExpr(st, arg));
              }
              const argsStmt = buildArgsStmt({
                EXPRESSION: t.arrayExpression(exprs)
              })
              path.insertBefore(argsStmt);
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
                    const leftExpr = origExpr.left;

                    // TODO(Chris): what on earth are babel "Patterns"?
                    if (t.isIdentifier(leftExpr)) {
                      const varName = st_get(st, lvaltoName(origExpr.left));
                      const varUpdate = buildVarUpdate({
                        VARIABLE: varName,
                        EXPRESSION: reifyExpr(st, rightExpr)
                      });
                      path.insertBefore(varUpdate);
                    } else if (t.isMemberExpression(leftExpr)) {
                      const memberUpdate = buildVarUpdate({
                        VARIABLE: reifyExpr(st, leftExpr),
                        EXPRESSION: reifyExpr(st, rightExpr)
                      });
                      path.insertBefore(memberUpdate);
                    } else {
                      throw new Error('left side of expression statment is not supported');
                    }
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

function reifyExpr(st: S, expr: t.Expression): t.Expression {
  switch (expr.type) {
    case 'NumericLiteral': return buildNumericExpr({ EXPRESSION: expr });
    case 'BooleanLiteral': return buildBooleanExpr({ EXPRESSION: expr });
    case 'StringLiteral': return buildStringExpr({ EXPRESSION: expr });
    case 'Identifier': return reifyIdentifier(st, expr);
    case 'CallExpression': return reifyCallExpression(st, expr);
    case 'LogicalExpression': return reifyLogicalExpression(st, expr);
    case 'UnaryExpression': return reifyUnaryExpression(st, expr);
    case 'BinaryExpression': return reifyBinaryExpression(st, expr);
    case 'ConditionalExpression': return reifyConditionalExpression(st, expr);
    case 'ObjectExpression': return reifyObjectExpression(st, expr);
    case 'MemberExpression': return reifyMemberExpression(st, expr);
    default: throw new Error(`unsupported expression type: ${expr.type}`);
  }
}

function reifyIdentifier(st: S, expr: t.Identifier): t.Expression {
  let x = st_get(st, expr.name);
  if (x === undefined) {
      console.log(expr);
      throw new Error(`no binding for ${expr.name}`);
  }
  return t.identifier(x);
}

function reifyCallExpression(st: S, expr: t.CallExpression): t.Expression {
  return t.sequenceExpression(expr.arguments.map(e =>
    buildArgumentExpr({ EXPRESSION: reifyExpr(st, e as t.Expression) })));
}

function reifyLogicalExpression(st: S, expr: t.LogicalExpression): t.Expression {
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

function reifyUnaryExpression(st: S, expr: t.UnaryExpression): t.Expression {
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

function reifyBinaryExpression(st: S, expr: t.BinaryExpression): t.Expression {
  const e1 = reifyExpr(st, expr.left);
  const e2 = reifyExpr(st, expr.right);
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
  });
  return binaryExpr;
}

function reifyConditionalExpression(st: S, expr: t.ConditionalExpression): t.Expression {
  const test = reifyExpr(st, expr.test);
  const consequent = reifyExpr(st, expr.consequent);
  const alternate = reifyExpr(st, expr.alternate);
  const ternaryExpr = buildTernaryExpr({
    EXPRESSION1: test,
    EXPRESSION2: consequent,
    EXPRESSION3: alternate
  });
  return ternaryExpr;
}

function reifyObjectExpression(st: S, expr: t.ObjectExpression): t.Expression {
  const properties = expr.properties;
  const innerProperties : (t.ObjectMethod |
                           t.ObjectProperty |
                           t.SpreadElement)[] = [];
  for(let i=0; i<properties.length; i++) {
    const property = properties[i];
    switch (property.type) {
      case 'ObjectProperty': {
        const key = property.key;
        const value = property.value;
        if(!t.isIdentifier(key)) {
          throw new Error("Only Identifier keys supported.");
        }
        if(!t.isExpression(value)) {
          throw new Error("Only Expression values supported.");
        }
        innerProperties.push(t.objectProperty(key, reifyExpr(st, value)));
        break;
      }
      default: throw new Error(`Unsupported property.type: ${property.type}.`);
    }
  }
  const objExpression = buildObjectExpr({
    EXPRESSION: t.objectExpression(innerProperties)
  })
  return objExpression;
}

// TODO(emily): Add functionality for { x : 1 }.x
function reifyMemberExpression(st: S, expr: t.MemberExpression): t.Expression {
  const object = reifyExpr(st, expr.object);
  const property = expr.property; // reifyExpr(st, expr.property);
  if(!t.isIdentifier(object)) {
    throw new Error("Only Identifier objects expected.");
  }
  if(!t.isIdentifier(property)) {
    throw new Error("Only Identifier properties expected.");
  }
  const memberExpression = buildMemberExpr({
    EXPRESSION1: t.identifier(object.name),
    EXPRESSION2: t.stringLiteral(property.name) // TODO(emily): Should these be Exp's? Say a function is used to calculate the field...
  })
  return memberExpression;
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
