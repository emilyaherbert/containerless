import * as helpers from '../helpers';

test('test 1', () => {
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