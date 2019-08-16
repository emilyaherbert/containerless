import * as helpers from '../helpers';

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
  helpers.run_test(code, input);
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
  helpers.run_test(code, input);
});