import * as parser from '@babel/parser';
import generator from '@babel/generator';
import * as fs from 'fs';
import * as r from './insertTracing';

let inputCode = fs.readFileSync(process.argv[2], { encoding: 'utf-8' });
let ast = parser.parse(inputCode);

ast.program.body = r.reifyStatements(ast.program.body);
console.log(generator(ast.program).code);