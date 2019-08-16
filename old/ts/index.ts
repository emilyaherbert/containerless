import * as babel from '@babel/core';
import * as fs from 'fs';
import * as t from '@babel/types';
import * as traverse from '@babel/traverse';
import * as template from '@babel/template';
import * as insertTracing from './insertTracing';

const $T = require('../dist/runtime');
const $R = require('../native');

const code = fs.readFileSync('input.js', { encoding: 'utf-8' });
const ast = babel.parseSync(code);

if(ast === null) {
  throw new Error("Found null ast.");
} else if(!t.isFile(ast)) {
  throw new Error("Expected file ast.");
}

let trace = insertTracing.transform(code);
let func_output = eval(trace)(0);

let classes = $T.getClasses();
let program = $T.getProgram();

let programStr = $T.getProgramAsString();
$R.compile(programStr);
let result = $R.run();
console.log(result);