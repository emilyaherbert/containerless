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

test('test', () => {
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
});

test('multiple if statements', () => {
  $T.clear();

  let code = `
  function F(x) {
    if (x <= 1) {
      return 1;
    } else if (x > 30) {
      return -3;
    } else {
      return x * (x - 1);
    }
  }
  F`;

  let f_input = 33;
  let i_input = $T.num(f_input);

  let t = insertTracing.transform(code);
  let f = eval(t)(f_input);
  let i = interp.eval($T.program_(), i_input);
  
  expect(f).toEqual(unwrap_num(i));
});

test('logical expression', () => {
  $T.clear();

  let code = `
  function F(x) {
    if (true && true) {
      return 1;
    } else {
      return 0;
    }
  }
  F`;

  let f_input = -3;
  let i_input = $T.num(f_input);

  let t = insertTracing.transform(code);
  let f = eval(t)(f_input);
  let i = interp.eval($T.program_(), i_input);
  
  expect(f).toEqual(unwrap_num(i));
});

test('logical expression 2', () => {
  $T.clear();

  let code = `
  function F(x) {
    let y = x + 1;
    if (x < y && y < x) {
      return 1;
    } else {
      return 0;
    }
  }
  F`;

  let f_input = -3;
  let i_input = $T.num(f_input);

  let t = insertTracing.transform(code);
  let f = eval(t)(f_input);
  let i = interp.eval($T.program_(), i_input);
  
  expect(f).toEqual(unwrap_num(i));
});

test('logical expression 3', () => {
  $T.clear();

  let code = `
  function F(x) {
    let y = x + 1;
    if (x < y || y < x) {
      return 1;
    } else {
      return 0;
    }
  }
  F`;

  let f_input = -3;
  let i_input = $T.num(f_input);

  let t = insertTracing.transform(code);
  let f = eval(t)(f_input);
  let i = interp.eval($T.program_(), i_input);
  
  expect(f).toEqual(unwrap_num(i));
});

// TODO(Chris): Assignment does not work if trying to assign over the 'input' variable
test('assignment 1', () => {
  $T.clear();

  let code = `
  function F(x) {
    let y = 1;
    if (x < 0) {
        y = 2;
    }
    return y;
  }
  F`;

  let f_input = -3;
  let i_input = $T.num(f_input);

  let t = insertTracing.transform(code);
  let f = eval(t)(f_input);
  let i = interp.eval($T.program_(), i_input);
  
  expect(f).toEqual(unwrap_num(i));
});

test('assignment 2', () => {
  $T.clear();

  let code = `
  function F(x) {
    let y = 1
    y = 2;
    return y;
  }
  F`;

  let f_input = 3;
  let i_input = $T.num(f_input);

  let t = insertTracing.transform(code);
  let f = eval(t)(f_input);
  let i = interp.eval($T.program_(), i_input);
  
  expect(f).toEqual(unwrap_num(i));
});

test('assignment 3', () => {
  $T.clear();

  let code = `
  function F(x) {
    x = 2;
    return x;
  }
  F`;

  let f_input = 3;
  let i_input = $T.num(f_input);

  let t = insertTracing.transform(code);
  let f = eval(t)(f_input);
  let i = interp.eval($T.program_(), i_input);
  
  expect(f).toEqual(unwrap_num(i));
});

test('if no else 1', () => {
  $T.clear();

  let code = `
  function F(x) {
    if(true) {
      return 42;
    }
    return 24;
  }
  F`;

  let f_input = 0;
  let i_input = $T.num(f_input);

  let t = insertTracing.transform(code);
  let f = eval(t)(f_input);
  let i = interp.eval($T.program_(), i_input);
  
  expect(f).toEqual(unwrap_num(i));
});

test('if no else 2', () => {
  $T.clear();

  let code = `
  function F(x) {
    if(false) {
      return 42;
    }
    return 24;
  }
  F`;

  let f_input = 0;
  let i_input = $T.num(f_input);

  let t = insertTracing.transform(code);
  let f = eval(t)(f_input);
  let i = interp.eval($T.program_(), i_input);
  
  expect(f).toEqual(unwrap_num(i));
});

test('function argument', () => {
  $T.clear();

  let code = `
  function F(x) {
    if(false) {
      return 42;
    }
    return 24;
  }
  let y = 10;
  F(y)
  `;

  let i_input = $T.num(10);

  let t = insertTracing.transform(code);
  let f = eval(t);
  let i = interp.eval($T.program_(), i_input);
  
  expect(f).toEqual(unwrap_num(i));
});

// TODO(emily): Should this be in runtime.ts? Really only useful for testing...
function unwrap_num(e: Exp): number {
  switch(e.kind) {
    case 'number': return e.value;
    default: throw "Expected number in unwrap_num."
  }
}
