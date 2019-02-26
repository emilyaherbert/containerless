import * as helpers from '../helpers';

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
  helpers.run_test(code, input);
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
  helpers.run_test(code, input);
});