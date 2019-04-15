import * as babel from '@babel/core';
import * as fs from 'fs';
import * as t from '@babel/types';
import * as traverse from '@babel/traverse';
import * as template from '@babel/template';
import * as insertTracing from './insertTracing';
const $T = require('../dist/runtime');

const code = fs.readFileSync('input.js', { encoding: 'utf-8' });
const ast = babel.parseSync(code);

if (ast === null) {
    throw 'something wrong';
}
if (!t.isFile(ast)) {
    throw 'something wrong';
}

// const result = babel.transformSync(code, {
//     plugins: [ insertTracing.plugin ],
//     ast: true
// });

let trace = insertTracing.transform(code);
let func_output = eval(trace)(0);
// let classes = $T.getClasses();
let program = $T.getProgram();
// let ast_output = interp.eval(program, wrap_exp(input));
// expect(ast_output).toEqual(wrap_exp(func_output));

$T.log();
// console.log(program);
// console.log(result!.ast);
// console.log(result!.code);
