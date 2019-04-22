import * as helpers from '../helpers';

test('test 0', () => {
  let code = `
  function main(x) {
    let y = 1;
    return y + 1;
  }
  main
  `;

  let input = 10;
  helpers.run_linked_test(code, input);
});

// TODO(emily): Deal with input.
test('test 1', () => {
  let code = `
  function main(x) {
    let y = 1 + 1;
    return 2 + 3 + x + y;
  }
  main
  `;

  let input = 10;
  helpers.run_linked_test(code, input);
});

// TODO(emily): Fails in Rust if every branch is not explored.
// It expects both branches to return the same type.
/*
test('simple if', () => {
  let code = `
  function main(x) {
    if (true) {
      return 1;
    }
  }
  main
  `;

  let input = 33;
  helpers.run_linked_test(code, input);
});
*/

test('simple if 2', () => {
  let code = `
  function main(x) {
    if (x < 0) {
      return 1;
    } else {
      return 0;
    }
  }
  main
  `;

  let inputs = [-10, 10];
  helpers.run_linked_tests(code, inputs);
});

// NOTE(emily): Um does Rust not have order of operations?
test('multiple if statements', () => {
  let code = `
  function main(x) {
    if (x <= 1) {
      return 1;
    } else if (x > 30) {
      return -3;
    } else {
      return x * (x - 2);
    }
  }
  main
  `;

  let inputs = [0, 31, 10];
  helpers.run_linked_tests(code, inputs);
});