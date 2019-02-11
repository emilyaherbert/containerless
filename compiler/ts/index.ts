import * as babel from '@babel/core';
import * as fs from 'fs';
import * as t from '@babel/types';
import * as traverse from '@babel/traverse';
import * as template from '@babel/template';

const code = fs.readFileSync('input.js', { encoding: 'utf-8' });
const ast = babel.parseSync(code);

if (ast === null) {
    throw 'something wrong';
}
if (!t.isFile(ast)) {
    throw 'something wrong';
}

const buildBinding = template.statement(`
    let VARIABLE_NAME = $T.bind(EXPRESSION);
`);


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

            let buildRequireStmt = template.statement(`let VARIABLE = require('./dist/runtime');`);
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
    // TODO(Chriscbr): Recursing causes traversal to go on forever...
    // VariableDeclaration: {
    //     exit(path: traverse.NodePath<t.VariableDeclaration>, st) {
    //         console.log(path.node);
    //         const declarations = path.node.declarations;
    //         for (let i = 0; i < declarations.length; ++i) {
    //             const id = declarations[i].id;
    //             const expr = declarations[i].init;
    //             const buildVarDeclaration = template.statement(
    //                 `let VARIABLE = $T.bind(EXPRESSION);`,
    //                 { placeholderPattern: /(VARIABLE)|(EXPRESSION)/ });
    //             const varDeclaration = buildVarDeclaration({
    //                 VARIABLE: id,
    //                 EXPRESSION: expr
    //             });
    //             path.insertBefore(varDeclaration);
    //             // path.skip(); // doesn't help
    //         }
    //     }
    // },
    ReturnStatement: {
        exit(path: traverse.NodePath<t.ReturnStatement>, st) {
            let innerExpr = path.node.argument;
            const buildReturnStatement = template.statement(
                `$T.return_(EXPRESSION);`,
                { placeholderPattern: /EXPRESSION/ });
            const returnStatement = buildReturnStatement(
                {EXPRESSION: innerExpr});
            path.insertBefore(returnStatement);
            path.getSibling(path.key as number - 1).traverse(exprVisitor, st);
        }
    }
};

const exprVisitor: traverse.Visitor<S> = {
    // TODO(Chriscbr): This recurses indefinitely...
    // NumericLiteral: {
    //     exit(path: traverse.NodePath<t.NumericLiteral>, st) {
    //         const buildLiteral = template.expression(
    //             `$T.num(VALUE)`,
    //             { placeholderPattern: /VALUE/ });
    //         const literal = buildLiteral({ VALUE: t.numericLiteral(path.node.value) });
    //         path.replaceWith(literal);
    //     }
    // },
    Identifier: {
        exit(path: traverse.NodePath<t.Identifier>, st) {
            const currName = path.node.name;
            if (st.names.has(currName)) {
                path.node.name = st.names.get(currName)!;
            } else {
                // TODO(Chriscbr): throw an error if an identifier is used in an expression
                // that hasn't already been declared (or imported) somewhere?
            }
        }
    },
    BinaryExpression: {
        exit(path: traverse.NodePath<t.BinaryExpression>, st) {
            const leftExpr = path.node.left;
            const rightExpr = path.node.right;
            const op = path.node.operator;
            if (op === '+') {
                const buildBinOp = template.expression(
                    `$T.add(LEFT, RIGHT)`,
                    { placeholderPattern: /(LEFT)|(RIGHT)/ }
                );
                path.replaceWith(
                    buildBinOp({LEFT: leftExpr, RIGHT: rightExpr}) as t.BinaryExpression);
            } else {
                return;
            }
        }
    }
}

function plugin() {
    return { visitor: visitor };
}


const result = babel.transformSync(code, {
    plugins: [ plugin ],
    ast: true
});

// console.log(result!.ast);
console.log(result!.code);
