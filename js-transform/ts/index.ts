import * as parser from '@babel/parser';
import generator from '@babel/generator';
import * as fs from 'fs';
import * as r from './insertTracing';
import * as n from '@stopify/normalize-js';
import { Map } from 'immutable';

let inputCode = fs.readFileSync(process.argv[2], { encoding: 'utf-8' });
let normalized = n.normalize(inputCode);
let ast = parser.parse(normalized);


const [body, st] = r.reifyStatements(ast.program.body, Map());
ast.program.body = body
console.log(generator(ast.program).code);