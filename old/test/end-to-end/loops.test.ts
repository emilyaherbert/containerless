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

test('nested loops 1', () => {
  let code = `
  function main(x) {
    let count = 0;
    let countUp = 0;
    while(countUp < 100) {

      let countDown = 100;
      while(countDown > 0) {

        let countLeft = 100;
        while(countLeft > 0) {

          let countRight = 0;
          while(countRight < 100) {
            count = count + 1;
            countRight = countRight + 4;
          }
          countLeft = countLeft - 3;
        }
        countDown = countDown - 2;
      }
      countUp = countUp + 1;
    }
    return count;
  }

  main
  `;

  let input = 100;
  helpers.run_test(code, input);
});