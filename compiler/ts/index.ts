import * as babel from '@babel/core';
import * as fs from 'fs';
import * as t from '@babel/types';
import * as traverse from '@babel/traverse';
import * as template from '@babel/template';
import * as insertTracing from './insertTracing';

const code = fs.readFileSync('input.js', { encoding: 'utf-8' });
const ast = babel.parseSync(code);

if (ast === null) {
    throw 'something wrong';
}
if (!t.isFile(ast)) {
    throw 'something wrong';
}



const result = babel.transformSync(code, {
    plugins: [ insertTracing.plugin ],
    ast: true
});

// console.log(result!.ast);
console.log(result!.code);
