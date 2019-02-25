const $T = require('../dist/runtime');
import { Interpreter } from '../ts/interpreter';
import * as insertTracing from '../ts/insertTracing';
import { Exp } from "../ts/types";

let interp = new Interpreter();

function run_test(code: string, input: string | boolean | number) {
  $T.clear();
  // NOTE(arjun): We could also run 'eval(code)(input)', but we are not. We
  // are assuming that the tracing does not change the semantics of 'code'.
  let trace = insertTracing.transform(code);
  console.log(trace);
  let func_output = eval(trace)(input);
  $T.log();
  let ast_output = interp.eval($T.program_(), wrap_exp(input));
  expect(ast_output).toEqual(wrap_exp(func_output));
}

function wrap_exp(v : number | string | boolean): Exp {
  switch (typeof v) {
    case 'string': return $T.str(v);
    case 'number': return $T.num(v);
    case 'boolean': return $T.bool(v);
    default: throw "Found unexpected type in wrap_exp."
  }
}

// TODO(arjun): Just use wrap_exp and .toEqual

test('test', () => {
  let code = `
  function main(x) {
    let y = 1 + 1;
    return 2 + 3  + x + y;
  }
  main
  `;

  let input = 10;
  run_test(code, input);
});

test('multiple if statements', () => {
  let code = `
  function main(x) {
    if (x <= 1) {
      return 1;
    } else if (x > 30) {
      return -3;
    } else {
      return x * (x - 1);
    }
  }
  main
  `;

  let input = 33;
  run_test(code, input);
});

test('if no else 1', () => {
  let code = `
  function main(x) {
    if(true) {
      return 42;
    }
    return 24;
  }
  main
  `;

  let input = 0;
  run_test(code, input);
});

test('if no else 2', () => {
  let code = `
  function main(x) {
    if(false) {
      return 42;
    }
    return 24;
  }
  main
  `;

  let input = 0;
  run_test(code, input);
});

test('logical expression', () => {
  let code = `
  function main(x) {
    if (true && true) {
      return 1;
    } else {
      return 0;
    }
  }
  main
  `;

  let input = -3;
  run_test(code, input);
});

test('logical expression 2', () => {
  let code = `
  function main(x) {
    let y = x + 1;
    if (x < y && y < x) {
      return 1;
    } else {
      return 0;
    }
  }
  main
  `;

  let input = -3;
  run_test(code, input);
});

test('logical expression 3', () => {
  let code = `
  function main(x) {
    let y = x + 1;
    if (x < y || y < x) {
      return 1;
    } else {
      return 0;
    }
  }
  main
  `;

  let input = -3;
  run_test(code, input);
});

test('assignment 1', () => {
  let code = `
  function main(x) {
    let y = 1;
    if (x < 0) {
        y = 2;
    }
    return y;
  }
  main
  `;

  let input = -3;
  run_test(code, input);
});

test('assignment 2', () => {
  let code = `
  function main(x) {
    let y = 1
    y = 2;
    return y;
  }
  main
  `;

  let input = 3;
  run_test(code, input);
});

test('tricky scope 1', () => {
  let code = `
  function main(x) {
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
  main
  `;

  let input = 0;
  run_test(code, input);
});

test('tricky scope 2', () => {
  let code = `
  function main(x) {
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
  main
  `;

  let input = 0;
  run_test(code, input);
});

test('tricky scope 3', () => {
  let code = `
  function main(x) {
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
  main
  `;

  let input = 0;
  run_test(code, input);
});

test('tricky scope 4', () => {
  let code = `
  function main(x) {
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
  main
  `;

  let input = 0;
  run_test(code, input);
});

test('tricky scope 5', () => {
  let code = `
  function main(x) {
    let y = 0;
    let z = 100;
    if(y < z) {
      let y = 200;
    }
    return y;
  }
  main
  `;

  let input = 0;
  run_test(code, input);
});

test('tricky scope 6', () => {
  let code = `
  function main(x) {
    let y = 0;
    let z = 100;
    if(y < z) {
      y = 200;
    }
    return y;
  }
  main
  `;

  let input = 0;
  run_test(code, input);
});

test('call expression 1', () => {
  let code = `
  function F(x) {
    return x*2;
  }

  function main(x) {
    let y = 100;
    let z = F(y);
    return z;
  }

  main
  `;

  let input = 0;
  run_test(code, input);
});

test('call expression 2', () => {
  let code = `
  function F(x) {
    return x * 2;
  }

  function G(x) {
    return x * 10;
  }

  function main(x) {
    let y = F(x);
    let z = G(y);
    return z;
  }

  main
  `;

  let input = 5;
  run_test(code, input);
});

test('call expression 3', () => {
  let code = `
  function F(x) {
    return x * 2;
  }

  function G(x) {
    return x * 10;
  }

  function main(x) {
    let y = F(x);
    let z = G(y);
    return z;
  }

  main
  `;

  let input = -100;
  run_test(code, input);
});

test('call expression 4', () => {
  let code = `
  function F(x) {
    return x * 2;
  }

  function G(x) {
    return x * 10;
  }

  function main(x) {
    if(x < 0) {
      let y = F(x);
      return y + 10;
    } else {
      let y = G(x);
      return y + 10;
    }
  }

  main
  `;

  let input = -100;
  run_test(code, input);
});

test('call expression 5', () => {
  let code = `
  function F(x) {
    return x * 2;
  }

  function G(x) {
    return x * 10;
  }

  function main(x) {
    if(x < 0) {
      let y = F(x);
      return y + 10;
    } else {
      let y = G(x);
      return y + 10;
    }
  }

  main
  `;

  let input = 100;
  run_test(code, input);
});

test('call expression 5', () => {
  let code = `
  function F(x) {
    return x * 2;
  }

  function G(x) {
    return x * 10;
  }

  function main(x) {
    let z = 0;
    if(x < 0) {
      let y = F(x);
      z = y;
    } else {
      let y = G(x);
      z = y;
    }
    return z;
  }

  main
  `;

  let input = -100;
  run_test(code, input);
});

test('call expression 6', () => {
  let code = `
  function F(x) {
    return x * 2;
  }

  function G(x) {
    return x * 10;
  }

  function main(x) {
    let z = 0;
    if(x < 0) {
      let y = F(x);
      z = y;
    } else {
      let y = G(x);
      z = y;
    }
    return z;
  }

  main
  `;

  let input = 100;
  run_test(code, input);
});


test('call expression, multiple arguments 1', () => {
  let code = `
  function F(x, y) {
    return x + y;
  }

  function main(x) {
    let a = 100;
    let b = 100;
    let c = F(a, b);
    return c;
  }

  main
  `;

  let input = 100;
  run_test(code, input);
});

test('call expression, multiple arguments 2', () => {
  let code = `
  function F(x, y) {
    return x + y;
  }

  function main(x) {
    let a = 100;
    let b = 100;
    let c = F(a, b);
    return a + b + c;
  }

  main
  `;

  let input = 100;
  run_test(code, input);
});

test('while loops 1', () => {
  let code = `
  function main(x) {
    let i = 0;
    let y = 100;
    while(i < 10) {
      y = y + 100;
      i = i + 1;
    }
    return y;
  }

  main
  `;

  let input = 100;
  run_test(code, input);
});

test('while loops 2', () => {
  let code = `

  function adder(x, y) {
    return x + y;
  }

  function main(x) {
    let i = 0;
    let y = 100;
    while(i < 10) {
      let hundred = 100;
      let ret = adder(y, hundred);
      y = ret;
      i = i + 1;
    }
    return y;
  }

  main
  `;

  let input = 100;
  run_test(code, input);
});

test('recursion 1', () => {
  let code = `
  function factorial(x) {
    if(x === 0) {
      return 1;
    } else {
      let y = x - 1;
      let z = factorial(y);
      return x + z;
    }
  }

  function main(x) {
    let y = factorial(x);
    return y;
  }

  main
  `;

  let input = 5;
  run_test(code, input);
});

test('recursion 2', () => {
  let code = `
  function count_every_other(x, mod) {
    if (x === 0) {
      return 0;
    }
    if(mod) {
      let newX = x - 1;
      let newMod = false;
      let recur = count_every_other(newX, newMod);
      return 1 + recur;
    } else {
      let newX = x - 1;
      let newMod = true;
      let recur = count_every_other(newX, newMod);
      return recur;
    }
  }

  function main(x) {
    let mod = true;
    let y = count_every_other(x, mod);
    return y;
  }

  main
  `;

  let input = 5;
  run_test(code, input);
});

test('strings 1', () => {
  let code = `
  function main(x) {
    return "hello world!";
  }
  main
  `;

  let input = 0;
  run_test(code, input);
});

test('strings 2', () => {
  let code = `
  function main(x) {
    return "hello " + "world!";
  }
  main
  `;

  let input = 0;
  run_test(code, input);
});

// TODO(Chris): This will not work for now as long as we assume 'input'
// is a 'number' type in getTyp()
/*
test('strings 3', () => {
  let code = `
  function main(x) {
    return "hello " + x;
  }
  main
  `;

  let input = "arjun";
  run_test(code, input);
});
*/

test('ternary expression 1', () => {
  let code = `
  function main(x) {
    let y = x > 0 ? 1 : -1;
    return y;
  }
  main
  `;

  let input = -3;
  run_test(code, input);
});

test('ternary expression 2', () => {
  let code = `
  function main(x) {
    let y = x > 0 ? 1
      : x < -3 ? -1
      : 0;
    return y;
  }
  main
  `;

  let input = -1;
  run_test(code, input);
});
