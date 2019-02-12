const $T = require('../dist/runtime');
import { Interpreter } from '../ts/interpreter';
import * as insertTracing from '../ts/insertTracing';
import { Exp } from "../ts/types";

/*

  1. Insert $T statements with Babel.
  2. Eval code to generate AST.
  3. Eval AST.
  4. Compare result from eval'd code to eval'd AST.

*/


let interp = new Interpreter();

test('end-to-end goal', () => {
  $T.clear();

  let code = `
  function F(x) {
    let y = 1 + 1;
    return 2 + 3  + x + y;
  }
  F
  `;

  let f_input = 10;
  let i_input = $T.num(f_input);

  let t = insertTracing.transform(code);
  let f = eval(t)(f_input);
  let i = interp.eval($T.program_(), i_input);
  
  expect(f).toEqual(unwrap_num(i));
})

// TODO(emily): Should this be in runtime.ts? Really only useful for testing...
function unwrap_num(e: Exp): number {
  switch(e.kind) {
    case 'number': return e.value;
    default: throw "Expected number in unwrap_num."
  }
}