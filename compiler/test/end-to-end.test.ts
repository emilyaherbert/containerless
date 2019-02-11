import * as $T from '../ts/runtime';
import * as $I from '../ts/interpreter';

test('end-to-end goal', () => {
  $T.clear();

  let code = `
  function F(x) {
    let y = 1 + 1;
    return x + y;
  }
  F
  `

  // insert $T statements with babel
  // run code to generate AST
  // put AST through interpreter
  // check to see if result from original code matches result from AST interpreter

  return false;
})