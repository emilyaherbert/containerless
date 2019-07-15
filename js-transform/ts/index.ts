import * as parser from '@babel/parser';
import generator from '@babel/generator';
import * as t from '@babel/types';
import * as fs from 'fs';

let inputCode = fs.readFileSync(process.argv[2], { encoding: 'utf-8' });
let ast = parser.parse(inputCode);

ast.program.body.push(t.expressionStatement(t.stringLiteral('hello\'\'\' world')));
console.log(generator(ast.program).code);