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


const visitor: traverse.Visitor = {
    Program: {
        exit(path: traverse.NodePath<t.Program>) {
            const id = t.identifier('$T'); // TODO(arjun): capture

            let buildRequireStmt = template.statement(`let VARIABLE = require('./dist/runtime');`);
            let requireStmt = buildRequireStmt({
                VARIABLE: id
            });
            path.node.body.unshift(requireStmt);
        }
    },
    FunctionDeclaration: {
        exit(path: traverse.NodePath<t.FunctionDeclaration>) {
            const body = path.node.body.body;
            for (let i = 0; i < path.node.params.length; ++i) {
                const param = path.node.params[i];
                const newParam = path.scope.generateUidIdentifierBasedOnNode(param);
                let buildInputDeclaration = template.statement(
                    `let VARIABLE = $T.input();`,
                    { placeholderPattern: /VARIABLE/ }); // avoid $T being captured by
                let inputDeclaration = buildInputDeclaration({
                    VARIABLE: newParam
                });
                body.unshift(inputDeclaration);
            }
        }
    }
};

function plugin() {
    return { visitor: visitor };
}


const result = babel.transformSync(code, {
    plugins: [ plugin ],
    ast: true
});

// console.log(result!.ast);
console.log(result!.code);
