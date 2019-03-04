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

test('object modification 1', () => {
  let code = `
  function main(x) {
    let y = { a : 1 };
    y.a = 2;
    return y.a;
  }
  main
  `;

  let input = -1;
  helpers.run_test(code, input);
});

test('object modification 2', () => {
  let code = `
  function F(a) {
    return { x : 100, z : 1000 };
  }

  function main(x) {
    let a = 2;
    let y = F(a);
    y.z = 2000;
    return y.z;
  }
  main
  `;

  let input = -1;
  helpers.run_test(code, input);
});

test('object modification 3', () => {
  let code = `
  function F(a) {
    let b = { x : 100, z : 1000 };
    b.x = 200;
    return b;
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

test('objects in operations 1', () => {
  let code = `
  function main(x) {
    let y = { a : 1, b : 2 };
    return y.a + y.b;
  }
  main
  `;

  let input = -1;
  helpers.run_test(code, input);
});

test('objects in operations 2', () => {
  let code = `
  function main(x) {
    let y = { a : 1, b : 2 };
    return (y.a > 0);
  }
  main
  `;

  let input = -1;
  helpers.run_test(code, input);
});

test('objects in operations 2', () => {
  let code = `
  function main(x) {
    let y = { a : 1, b : 2, c : true };
    if (y.c) {
      return (y.a - y.b);
    } else {
      return (y.b - y.a);
    }
  }
  main
  `;

  let input = -1;
  helpers.run_test(code, input);
});