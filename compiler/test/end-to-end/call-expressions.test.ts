import * as helpers from '../helpers';

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
  helpers.run_test(code, input);
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
  helpers.run_test(code, input);
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
  helpers.run_test(code, input);
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
  helpers.run_test(code, input);
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
  helpers.run_test(code, input);
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
  helpers.run_test(code, input);
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
  helpers.run_test(code, input);
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
  helpers.run_test(code, input);
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
  helpers.run_test(code, input);
});