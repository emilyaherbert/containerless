import * as helpers from '../helpers';

test('objects 1', () => {
  let code = `
  function main(x) {
    let y = { a : 1 };
    return y.a;
  }
  main
  `;

  let input = -1;
  helpers.run_test(code, input);
});

test('objects 2', () => {
  let code = `
  function main(x) {
    let y = { a : 1, b : 2 };
    return y.a;
  }
  main
  `;

  let input = -1;
  helpers.run_test(code, input);
});

test('objects 3', () => {
  let code = `
  function main(x) {
    let y = { a : 1, b : 2 };
    return y.b;
  }
  main
  `;

  let input = -1;
  helpers.run_test(code, input);
});

test('objects 4', () => {
  let code = `
  function F(a) {
    return { x : 100, z : 1000 };
  }

  function main(x) {
    let a = 2;
    let y = F(a);
    return y.x;
  }
  main
  `;

  let input = -1;
  helpers.run_test(code, input);
});

test('objects 5', () => {
  let code = `
  function F(a) {
    return { x : 100, z : 1000 };
  }

  function main(x) {
    let a = 2;
    let y = F(a);
    return y.z;
  }
  main
  `;

  let input = -1;
  helpers.run_test(code, input);
});