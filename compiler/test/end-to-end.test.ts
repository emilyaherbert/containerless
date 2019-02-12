const $T = require('../dist/runtime');
import * as $I from '../ts/interpreter';
import * as insertTracing from '../ts/insertTracing';

test('end-to-end goal', () => {
  // $T.clear();

  let code = `
  function F(x) {
    let y = 1 + 1;
    return 2 + 3  + x + y;
  }
  F
  `;

  let r0 = insertTracing.transform(code);
  let r1 = eval(r0);
  console.log(r0);
  console.log(r1(10));

  let r2 = $T.program_()
  console.log(r2);
  $T.log();


  // insert $T statements with babel
  // run code to generate AST
  // put AST through interpreter
  // check to see if result from original code matches result from AST interpreter


})