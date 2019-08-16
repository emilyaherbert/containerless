import * as helpers from '../helpers';

test('test 1', () => {
  let code = `
  function main(x) {
    let y = 1 + 1;
    return 2 + 3  + x + y;
  }
  main
  `;

  let input = 10;
  helpers.run_test(code, input);
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
  helpers.run_test(code, input);
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
  helpers.run_test(code, input);
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
  helpers.run_test(code, input);
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
  helpers.run_test(code, input);
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
  helpers.run_test(code, input);
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
  helpers.run_test(code, input);
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
  helpers.run_test(code, input);
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
  helpers.run_test(code, input);
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
  helpers.run_test(code, input);
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
  helpers.run_test(code, input);
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
  helpers.run_test(code, input);
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
  helpers.run_test(code, input);
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
  helpers.run_test(code, input);
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
  helpers.run_test(code, input);
});

test('strings 1', () => {
  let code = `
  function main(x) {
    return "hello world!";
  }
  main
  `;

  let input = 0;
  helpers.run_test(code, input);
});

test('strings 2', () => {
  let code = `
  function main(x) {
    return "hello " + "world!";
  }
  main
  `;

  let input = 0;
  helpers.run_test(code, input);
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
  helpers.run_test(code, input);
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
  helpers.run_test(code, input);
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
  helpers.run_test(code, input);
});

test('void expression', () => {
  let code = `
  function main(x) {
    return void 3;
  }
  main
  `;

  let input = -1;
  helpers.run_test(code, input);
});
