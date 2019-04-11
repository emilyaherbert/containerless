import * as helpers from './helpers';

test('multiple inputs 1', () => {
  let code = `
  function F(x) {
    return x * 2;
  }

  function main(x) {
    let z = F(x);
    return z;
  }

  main
  `;
  
  let input = [100, -100];
  helpers.run_tests(code, input);
});

test('multiple inputs 2', () => {
  let code = `
  function main(x) {
    let y = x + 1;
    if(y > 0) {
      return x * 2;
    } else {
      return x * 4;
    }
  }

  main
  `;
  
  let input = [100, -100];
  helpers.run_tests(code, input);
});

test('multiple inputs 3', () => {
  let code = `
  function main(x) {
    if(x > 0) {
      return x * 2;
    } else if (x === 0) {
      return x + 100;
    } else {
      return x * 4;
    }
  }

  main
  `;
  
  let input = [100, -200, 0, 100, 12, 2, 3, -432432, 0, -432, -5, 432];
  helpers.run_tests(code, input);
});

test('multiple inputs 4', () => {
  let code = `
  function main(x) {
    if(x > 0) {
      if(x > 10) {
        return x * 2;
      } else {
        return x * 3;
      }
    } else {
      return x;
    }
  }

  main
  `;
  
  let input = [100, -200, 0, 100, 12, 2, 3, -432432, 0, -432, -5, 432];
  helpers.run_tests(code, input);
});