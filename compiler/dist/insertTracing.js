"use strict";
exports.__esModule = true;
var babel = require("@babel/core");
var t = require("@babel/types");
var template = require("@babel/template");
function mkExpr(code) {
    var tmpl = template.expression(code, { placeholderPattern: /(EXPRESSION\d?)/ });
    return function (binds) {
        var expr = tmpl(binds);
        expr.__runtime__ = true;
        return expr;
    };
}
// Converts string into a statement that can be avoided by visitors
// by checking the __runtime__ property.
function mkStmt(code) {
    var tmpl = template.statement(code, { placeholderPattern: /(VARIABLE)|(EXPRESSION)/ });
    return function (binds) {
        var stmt = tmpl(binds);
        stmt.__runtime__ = true;
        return stmt;
    };
}
var buildNumericExpr = mkExpr("$T.num(EXPRESSION)");
var buildBooleanExpr = mkExpr("$T.bool(EXPRESSION)");
var buildStringExpr = mkExpr("$T.str(EXPRESSION)");
var buildUnaryExpr = ['neg', 'plus', 'not', 'bitnot', '_void']
    .reduce(function (ret, elem) {
    ret[elem] = mkExpr("$T." + elem + "(EXPRESSION)");
    return ret;
}, {});
var buildLogicalExpr = ['and', 'or']
    .reduce(function (ret, elem) {
    ret[elem] = mkExpr("$T." + elem + "(EXPRESSION1, EXPRESSION2)");
    return ret;
}, {});
var buildBinaryExpr = ['add', 'gt', 'lt', 'geq', 'leq', 'sub', 'div', 'mul', 'remainder', 'pow', 'bitand', 'bitor', 'bitxor', 'rshift', 'unsignedrs', 'lshift', 'lshift', 'eq', 'ineq', 'exacteq', 'exactineq']
    .reduce(function (ret, elem) {
    ret[elem] = mkExpr("$T." + elem + "(EXPRESSION1, EXPRESSION2)");
    return ret;
}, {});
var buildTernaryExpr = mkExpr("$T.ternary(EXPRESSION1, EXPRESSION2, EXPRESSION3)");
var buildArgumentExpr = mkExpr("$T.argument(EXPRESSION)");
var buildObjectExpr = mkExpr("$T.object(EXPRESSION)");
var buildMemberExpr = mkExpr("$T.member(EXPRESSION1, EXPRESSION2)");
var buildMemberUpdateStmt = mkStmt("$T.updateObject(VARIABLE, EXPRESSION);");
var buildVarDeclaration = mkStmt("let VARIABLE = $T.bind(EXPRESSION);");
var buildVarUpdate = mkStmt("$T.update(VARIABLE, EXPRESSION);");
var buildInputDeclaration = mkStmt("let VARIABLE = $T.input();");
var buildExpectReturnDeclaration = mkStmt("let VARIABLE = $T.expectReturn();");
var buildStartTraceStmt = mkStmt("$T.startTrace();");
var buildRequireStmt = mkStmt("let VARIABLE = require('../dist/runtime');");
var buildIfStmt = mkStmt("$T.if_(EXPRESSION);");
var buildIfElseStmt = mkStmt("$T.ifElse(EXPRESSION);");
var buildEnterIfStmt = mkStmt("$T.enterIf(EXPRESSION);");
var buildExitIfStmt = mkStmt("$T.exitIf();");
// TODO(emily): It would be more efficient to have:
// 
// $T.enterWhile(foo);
// $T.enterBlock();
//  some code
// $T.exitBlock();
//
// $T.if_(bar):
// $T.enterBlock();
//  some code
// $T.exitBlock();
//
// This generates a problem in the runtime though, with looking back at previous statements.
var buildWhileStmt = mkStmt("$T.while_(EXPRESSION);");
var buildEnterWhileStmt = mkStmt("$T.enterWhile();");
var buildExitWhileStmt = mkStmt("$T.exitWhile();");
var buildReturnStmt = mkStmt("$T.return_(EXPRESSION);");
var buildArgsStmt = mkStmt("$T.args(EXPRESSION);");
var buildParamsStmt = mkStmt("let EXPRESSION = $T.params();");
var eUndefined = t.unaryExpression('void', t.numericLiteral(0));
function st_clear(st) {
    st.names = new Map([]);
    st.stack = [];
    return st;
}
function st_set(st, k, v) {
    st.names.set(k, v);
    return st;
}
function st_get(st, k) {
    if (st.names.has(k)) {
        return st.names.get(k);
    }
    for (var i = st.stack.length - 1; i > (-1); i--) {
        if (st.stack[i].has(k)) {
            return st.stack[i].get(k);
        }
    }
}
function st_push(st) {
    st.stack.push(new Map(st.names));
    st.names = new Map([]);
    return st;
}
function st_pop(st) {
    if (st.stack.length > 0) {
        st.names = st.stack.pop();
        return st;
    }
    else {
        throw "Found unexpected empty st.stack in st_pop.";
    }
}
var visitor = {
    Program: {
        enter: function (path, st) {
            st_clear(st);
        },
        exit: function (path, st) {
            var startTraceStmt = buildStartTraceStmt({});
            path.node.body.unshift(startTraceStmt);
            var id = t.identifier('$T'); // TODO(arjun): capture
            var requireStmt = buildRequireStmt({
                VARIABLE: id
            });
            path.node.body.unshift(requireStmt);
        }
    },
    FunctionDeclaration: {
        enter: function (path, st) {
            if (path.node.__runtime__) {
                path.skip();
                return;
            }
            var params = path.node.params;
            for (var i = 0; i < params.length; ++i) {
                var param = params[i];
                if (param.type !== 'Identifier') {
                    throw new Error('only identifier params supported');
                }
                var newParam = path.scope.generateUidIdentifierBasedOnNode(param);
                st_set(st, param.name, newParam.name);
            }
        },
        exit: function (path, st) {
            var name = path.node.id;
            var body = path.node.body.body;
            var params = path.node.params;
            // TODO(emily): Change hard-coded main.
            if (name === null) {
                throw new Error("Function name cannot be null.");
            }
            if (name.name === 'main') {
                for (var i = 0; i < params.length; ++i) {
                    var param = params[i];
                    if (!t.isIdentifier(param)) {
                        throw new Error('only identifier params supported');
                    }
                    var inputDeclaration = buildInputDeclaration({
                        VARIABLE: st_get(st, param.name)
                    });
                    body.unshift(inputDeclaration);
                }
            }
            else {
                var param_names = [];
                for (var i = 0; i < params.length; ++i) {
                    var param = params[i];
                    if (!t.isIdentifier(param)) {
                        throw new Error('only identifier params supported');
                    }
                    var name_1 = st_get(st, param.name);
                    if (name_1 !== undefined) {
                        param_names[i] = t.identifier(name_1);
                    }
                    else {
                        throw new Error("Found undefined name in FunctionDeclaration.");
                    }
                }
                var paramsStmt = buildParamsStmt({
                    EXPRESSION: t.arrayPattern(param_names)
                });
                body.unshift(paramsStmt);
            }
        }
    },
    IfStatement: {
        enter: function (path, st) {
            if (path.node.__runtime__) {
                path.skip();
                return;
            }
            var innerExpr = path.node.test;
            var consequent = path.node.consequent;
            var alternate = path.node.alternate;
            var enterIfTrueStmt = buildEnterIfStmt({
                EXPRESSION: t.booleanLiteral(true)
            });
            var enterIfFalseStmt = buildEnterIfStmt({
                EXPRESSION: t.booleanLiteral(false)
            });
            var exitIfStmt = buildExitIfStmt({});
            if (alternate === null) {
                // If statement.
                var ifStmt = buildIfStmt({
                    EXPRESSION: reifyExpr(st, innerExpr)
                });
                if (!t.isBlockStatement(consequent)) {
                    path.node.consequent = t.blockStatement([consequent]);
                }
                path.insertBefore(ifStmt);
                path.node.consequent.body.unshift(enterIfTrueStmt);
                path.node.consequent.body.push(exitIfStmt);
            }
            else {
                // IfElse statement.
                var ifElseStmt = buildIfElseStmt({
                    EXPRESSION: reifyExpr(st, innerExpr)
                });
                if (!t.isBlockStatement(consequent)) {
                    path.node.consequent = t.blockStatement([consequent]);
                }
                if (!t.isBlockStatement(alternate)) {
                    path.node.alternate = t.blockStatement([alternate]);
                }
                path.insertBefore(ifElseStmt);
                path.node.consequent.body.unshift(enterIfTrueStmt);
                path.node.consequent.body.push(exitIfStmt);
                path.node.alternate.body.unshift(enterIfFalseStmt);
                path.node.alternate.body.push(exitIfStmt);
            }
        }
    },
    WhileStatement: {
        enter: function (path, st) {
            if (path.node.__runtime__) {
                path.skip();
                return;
            }
            var test = path.node.test;
            var body = path.node.body;
            var whileStmt = buildWhileStmt({
                EXPRESSION: reifyExpr(st, test)
            });
            if (!t.isBlockStatement(body)) {
                path.node.body = t.blockStatement([body]);
            }
            var enterWhileStmt = buildEnterWhileStmt({});
            var exitWhileStmt = buildExitWhileStmt({});
            path.insertBefore(whileStmt);
            path.node.body.body.unshift(enterWhileStmt);
            path.node.body.body.push(exitWhileStmt);
        }
    },
    VariableDeclaration: {
        enter: function (path, st) {
            if (path.node.__runtime__) {
                path.skip();
                return;
            }
            if (path.node.declarations.length !== 1) {
                throw new Error('expected exactly one declaration');
            }
            var declaration = path.node.declarations[0];
            var id = declaration.id;
            var newId = path.scope.generateUidIdentifierBasedOnNode(id);
            st_set(st, lvaltoName(id), newId.name);
            var expr = declaration.init || eUndefined;
            if (t.isCallExpression(expr)) {
                var args = expr.arguments;
                var exprs = [];
                for (var i = 0; i < args.length; i++) {
                    var arg = args[i];
                    if (!t.isIdentifier(arg)) {
                        throw new Error('Only Identifier args supported.');
                    }
                    exprs.push(reifyExpr(st, arg));
                }
                var argsStmt = buildArgsStmt({
                    EXPRESSION: t.arrayExpression(exprs)
                });
                path.insertBefore(argsStmt);
                var expectReturnDeclaration = buildExpectReturnDeclaration({
                    VARIABLE: newId
                });
                path.insertAfter(expectReturnDeclaration);
            }
            else {
                var varDeclaration = buildVarDeclaration({
                    VARIABLE: newId,
                    EXPRESSION: reifyExpr(st, expr)
                });
                path.insertBefore(varDeclaration);
            }
        }
    },
    ExpressionStatement: {
        enter: function (path, st) {
            if (path.node.__runtime__) {
                path.skip();
                return;
            }
            if (t.isAssignmentExpression(path.node.expression)) {
                if (path.node.expression.operator === '=') {
                    var origExpr = path.node.expression;
                    var rightExpr = origExpr.right;
                    var leftExpr = origExpr.left;
                    // TODO(Chris): what on earth are babel "Patterns"?
                    if (t.isIdentifier(leftExpr)) {
                        var varName = st_get(st, lvaltoName(origExpr.left));
                        var varUpdate = buildVarUpdate({
                            VARIABLE: varName,
                            EXPRESSION: reifyExpr(st, rightExpr)
                        });
                        path.insertBefore(varUpdate);
                    }
                    else if (t.isMemberExpression(leftExpr)) {
                        var memberUpdateStmt = buildMemberUpdateStmt({
                            VARIABLE: reifyExpr(st, leftExpr),
                            EXPRESSION: reifyExpr(st, rightExpr)
                        });
                        path.insertBefore(memberUpdateStmt);
                    }
                    else {
                        throw new Error('left side of expression statment is not supported');
                    }
                }
            }
        }
    },
    BlockStatement: {
        enter: function (path, st) {
            if (path.node.__runtime__) {
                path.skip();
                return;
            }
            st_push(st);
        },
        exit: function (path, st) {
            st_pop(st);
        }
    },
    ReturnStatement: {
        enter: function (path, st) {
            if (path.node.__runtime__) {
                path.skip();
                return;
            }
            var innerExpr = path.node.argument;
            if (innerExpr === null) {
                innerExpr = eUndefined;
            }
            var returnStmt = buildReturnStmt({
                EXPRESSION: reifyExpr(st, innerExpr)
            });
            path.insertBefore(returnStmt);
        }
    }
};
function reifyExpr(st, expr) {
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
        default: throw new Error("unsupported expression type: " + expr.type);
    }
}
function reifyIdentifier(st, expr) {
    var x = st_get(st, expr.name);
    if (x === undefined) {
        console.log(expr);
        throw new Error("no binding for " + expr.name);
    }
    return t.identifier(x);
}
function reifyCallExpression(st, expr) {
    return t.sequenceExpression(expr.arguments.map(function (e) {
        return buildArgumentExpr({ EXPRESSION: reifyExpr(st, e) });
    }));
}
function reifyLogicalExpression(st, expr) {
    var e1 = reifyExpr(st, expr.left);
    var e2 = reifyExpr(st, expr.right);
    var traceOpName;
    switch (expr.operator) {
        case '&&':
            traceOpName = 'and';
            break;
        case '||':
            traceOpName = 'or';
            break;
        default:
            throw new Error("unsupported logical operator: " + expr.operator);
    }
    var logicalExpr = buildLogicalExpr[traceOpName]({
        EXPRESSION1: e1,
        EXPRESSION2: e2
    });
    return logicalExpr;
}
function reifyUnaryExpression(st, expr) {
    var e = reifyExpr(st, expr.argument);
    var traceOpName;
    switch (expr.operator) {
        case '-':
            traceOpName = 'neg';
            break;
        case '+':
            traceOpName = 'plus';
            break;
        case '!':
            traceOpName = 'not';
            break;
        case '~':
            traceOpName = 'bitnot';
            break;
        case 'void':
            traceOpName = '_void';
            break;
        default: // cases 'typeof', 'void', 'delete', 'throw' not handled
            throw new Error("unsupported binary operator: " + expr.operator);
    }
    var unaryExpr = buildUnaryExpr[traceOpName]({
        EXPRESSION: e
    });
    return unaryExpr;
}
function reifyBinaryExpression(st, expr) {
    var e1 = reifyExpr(st, expr.left);
    var e2 = reifyExpr(st, expr.right);
    var traceOpName;
    switch (expr.operator) {
        case '+':
            traceOpName = 'add';
            break;
        case '>':
            traceOpName = 'gt';
            break;
        case '<':
            traceOpName = 'lt';
            break;
        case '>=':
            traceOpName = 'geq';
            break;
        case '<=':
            traceOpName = 'leq';
            break;
        case '-':
            traceOpName = 'sub';
            break;
        case '/':
            traceOpName = 'div';
            break;
        case '*':
            traceOpName = 'mul';
            break;
        case '%':
            traceOpName = 'remainder';
            break;
        case '**':
            traceOpName = 'pow';
            break;
        case '&':
            traceOpName = 'bitand';
            break;
        case '|':
            traceOpName = 'bitor';
            break;
        case '^':
            traceOpName = 'bitxor';
            break;
        case '>>':
            traceOpName = 'rshift';
            break;
        case '>>>':
            traceOpName = 'unsignedrshift';
            break;
        case '<<':
            traceOpName = 'lshift';
            break;
        case '<<':
            traceOpName = 'lshift';
            break;
        case '==':
            traceOpName = 'eq';
            break;
        case '!=':
            traceOpName = 'ineq';
            break;
        case '===':
            traceOpName = 'exacteq';
            break;
        case '!==':
            traceOpName = 'exactineq';
            break;
        default: // cases 'in', and 'instanceof' not handled
            throw new Error("unsupported binary operator: " + expr.operator);
    }
    var binaryExpr = buildBinaryExpr[traceOpName]({
        EXPRESSION1: e1,
        EXPRESSION2: e2
    });
    return binaryExpr;
}
function reifyConditionalExpression(st, expr) {
    var test = reifyExpr(st, expr.test);
    var consequent = reifyExpr(st, expr.consequent);
    var alternate = reifyExpr(st, expr.alternate);
    var ternaryExpr = buildTernaryExpr({
        EXPRESSION1: test,
        EXPRESSION2: consequent,
        EXPRESSION3: alternate
    });
    return ternaryExpr;
}
function reifyObjectExpression(st, expr) {
    var properties = expr.properties;
    var innerProperties = [];
    for (var i = 0; i < properties.length; i++) {
        var property = properties[i];
        switch (property.type) {
            case 'ObjectProperty': {
                var key = property.key;
                var value = property.value;
                if (!t.isIdentifier(key)) {
                    throw new Error("Only Identifier keys supported.");
                }
                if (!t.isExpression(value)) {
                    throw new Error("Only Expression values supported.");
                }
                innerProperties.push(t.objectProperty(key, reifyExpr(st, value)));
                break;
            }
            default: throw new Error("Unsupported property.type: " + property.type + ".");
        }
    }
    var objExpression = buildObjectExpr({
        EXPRESSION: t.objectExpression(innerProperties)
    });
    return objExpression;
}
function reifyMemberExpression(st, expr) {
    var object = reifyExpr(st, expr.object);
    var property = expr.property; // reifyExpr(st, expr.property);
    if (!t.isIdentifier(object)) {
        throw new Error("Only Identifier objects expected.");
    }
    if (!t.isIdentifier(property)) {
        throw new Error("Only Identifier properties expected.");
    }
    var memberExpression = buildMemberExpr({
        EXPRESSION1: t.identifier(object.name),
        EXPRESSION2: t.stringLiteral(property.name) // TODO(emily): Should these be Exp's? Say a function is used to calculate the field...
    });
    return memberExpression;
}
/**
 * Given an 'LVal' that is an identifier, produces the identifier's name.
 * Throws an exception if the 'LVal' is not an identifier.
 *
 * @param lval an l-value
 * @returns the name of the identifier, if 'lval' is an identifier
 */
function lvaltoName(lval) {
    if (t.isIdentifier(lval)) {
        return lval.name;
    }
    else if (lval.type === 'RestElement' && lval.argument.type === 'Identifier') {
        return lval.argument.name;
    }
    else {
        throw new Error("Expected Identifier, received " + lval.type);
    }
}
function plugin() {
    return { visitor: visitor };
}
exports.plugin = plugin;
function transform(code) {
    var result = babel.transformSync(code, {
        babelrc: false,
        plugins: [plugin],
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
exports.transform = transform;
