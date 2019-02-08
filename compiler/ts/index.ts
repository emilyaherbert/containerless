import * as babel from '@babel/core';
import * as fs from 'fs';
import * as t from '@babel/types';
import * as traverse from '@babel/traverse';
import template from '@babel/template';

const code = fs.readFileSync('input.js', { encoding: 'utf-8' });
const ast = babel.parseSync(code);

if (ast === null) {
    throw 'something wrong';
}
if (!t.isFile(ast)) {
    throw 'something wrong';
}

const buildBinding = template(`
    let VARIABLE_NAME = $T.bind(EXPRESSION);
`);

const visitor: traverse.Visitor = {
    Program: {
        exit(path: traverse.NodePath<t.Program>) {
            if (path.node.body.length !== 0) {
                // possibly safer? needs to be tracked of / stored somewhere?
                // const newId = path.scope.generateUidIdentifier('$T');
                const id = t.identifier('$T');

                let buildRequireStmt = template(`let VARIABLE = require('./dist/runtime');`);
                let requireStmt = buildRequireStmt({
                    VARIABLE: id
                });

                // typescript giving a lot of errors here...
                // path.get('body').unshiftContainer('body', requireStmt);
                // path.node.body.unshiftContainer('body', requireStmt);
                (<traverse.NodePath>path.get('body.0')).insertBefore(requireStmt);
            }
        }
    },
    FunctionDeclaration: {
        exit(path: traverse.NodePath<t.FunctionDeclaration>) {
            if (path.node.body.body.length !== 0) {
                const firstStmt = (<traverse.NodePath>path.get('body.body.0'));
                for (let i = 0; i < path.node.params.length; ++i) {
                    const param = path.node.params[i];
                    const newParam = path.scope.generateUidIdentifierBasedOnNode(param);
                    let buildInputDeclaration = template(`let VARIABLE = $T.input();`);
                    let inputDeclaration = buildInputDeclaration({
                        VARIABLE: newParam
                    });
                    firstStmt.insertBefore(inputDeclaration);
                }
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
