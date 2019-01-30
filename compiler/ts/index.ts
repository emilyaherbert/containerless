import * as babel from '@babel/core';
import * as fs from 'fs';
import * as t from '@babel/types';
import * as traverse from '@babel/traverse';

const result = babel.parseSync(
    fs.readFileSync('input.js', { encoding: 'utf-8' }));

if (result === null) {
    throw 'something wrong';
}
if (!t.isFile(result)) {
    throw 'something wrong';
}

const visitor: traverse.Visitor = {
    CallExpression(path) {
        console.log(path.node.callee);
    }
};

function plugin() {
    return { visitor: visitor };
}


babel.transformFromAstSync(result, undefined, {
    plugins: [ plugin ]
});
// console.log(result);



