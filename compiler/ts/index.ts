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
