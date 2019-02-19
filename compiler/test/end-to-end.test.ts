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

test('test', () => {
  let code = `
  function F(x) {
    let y = 1 + 1;
    return 2 + 3  + x + y;
  }
  F
  `;

  let input = 10;
  run_test(code, input);
});

test('multiple if statements', () => {
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

  let input = 33;
  run_test(code, input);
});

test('logical expression', () => {
  let code = `
  function F(x) {
    if (true && true) {
      return 1;
    } else {
      return 0;
    }
  }
  F`;

  let input = -3;
  run_test(code, input);
});

test('logical expression 2', () => {
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

  let input = -3;
  run_test(code, input);
});

test('logical expression 3', () => {
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

  let input = -3;
  run_test(code, input);
});

// TODO(Chris): Assignment does not work if trying to assign over the 'input' variable
test('assignment 1', () => {
  let code = `
  function F(x) {
    let y = 1;
    if (x < 0) {
        y = 2;
    }
    return y;
  }
  F`;

  let input = -3;
  run_test(code, input);
});

test('assignment 2', () => {
  let code = `
  function F(x) {
    let y = 1
    y = 2;
    return y;
  }
  F`;

  let input = 3;
  run_test(code, input);
});

test('assignment 3', () => {
  let code = `
  function F(x) {
    x = 2;
    return x;
  }
  F`;

  let input = 3;
  run_test(code, input);
});

test('if no else 1', () => {
  let code = `
  function F(x) {
    if(true) {
      return 42;
    }
    return 24;
  }
  F`;

  let input = 0;
  run_test(code, input);
});

test('if no else 2', () => {
  let code = `
  function F(x) {
    if(false) {
      return 42;
    }
    return 24;
  }
  F`;

  let input = 0;
  run_test(code, input);
});

test('tricky scope 1', () => {
  let code = `
  function F(x) {
    let y = 0;
    if(true) {
      if(true) {
        y = 10;
      } else {
        y = 20;
      }
      y = y + 1;
    } else {
      if(true) {
        y = 30;
      } else {
        y = 40;
      }
      y = y + 1;
    }
    y = y + 1;
    return y;
  }
  F
  `;

  let input = 0;
  run_test(code, input);
});

test('tricky scope 2', () => {
  let code = `
  function F(x) {
    let y = 0;
    if(true) {
      if(false) {
        y = 10;
      } else {
        y = 20;
      }
      y = y + 1;
    } else {
      if(true) {
        y = 30;
      } else {
        y = 40;
      }
      y = y + 1;
    }
    y = y + 1;
    return y;
  }
  F
  `;

  let input = 0;
  run_test(code, input);
});

test('tricky scope 3', () => {
  let code = `
  function F(x) {
    let y = 0;
    if(false) {
      if(true) {
        y = 10;
      } else {
        y = 20;
      }
      y = y + 1;
    } else {
      if(true) {
        y = 30;
      } else {
        y = 40;
      }
      y = y + 1;
    }
    y = y + 1;
    return y;
  }
  F
  `;

  let input = 0;
  run_test(code, input);
});

test('tricky scope 4', () => {
  let code = `
  function F(x) {
    let y = 0;
    if(false) {
      if(true) {
        y = 10;
      } else {
        y = 20;
      }
      y = y + 1;
    } else {
      if(false) {
        y = 30;
      } else {
        y = 40;
      }
      y = y + 1;
    }
    y = y + 1;
    return y;
  }
  F
  `;

  let input = 0;
  run_test(code, input);
});

test('tricky scope 5', () => {
  let code = `
  function F(x) {
    let y = 0;
    let z = 100;
    if(y < z) {
      let y = 200;
    }
    return y;
  }
  F
  `;

  let input = 0;
  run_test(code, input);
});

test('tricky scope 6', () => {
  let code = `
  function F(x) {
    let y = 0;
    let z = 100;
    if(y < z) {
      y = 200;
    }
    return y;
  }
  F
  `;

  let input = 0;
  run_test(code, input);
});











let interp = new Interpreter();
function run_test(code: string, input: string | boolean | number) {
  $T.clear();

  let trace = insertTracing.transform(code);
  let func_output = eval(trace)(input);
  let ast_output = interp.eval($T.program_(), wrap_exp(input));

  expect(unwrap_exp(ast_output)).toBe(func_output);
}

function wrap_exp(v : number | string | boolean): Exp {
  switch (typeof v) {
    case 'string':
      return $T.str(v);
      break;
    case 'number':
      return $T.num(v);
      break;
    case 'boolean':
      return $T.bool(v);
      break;
    default: throw "Found unexpected type in wrap_exp."
  }
}

function unwrap_exp(e: Exp): string | boolean | number {
  switch(e.kind) {
    case 'string':
      return e.value;
      break;
    case 'number':
      return e.value;
      break;
    case 'boolean':
      return e.value;
      break;
    default: throw "Found unexpected e.king in unwrap_exp."
  }
}