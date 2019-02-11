import * as $T from '../ts/runtime';
import * as $I from '../ts/interpreter';

test('boolean', () => {
  $T.clear();

  let code = `
  function F() {
    $T.return_($T.bool(true));
    return true;
  }
  F
  `

  let f = eval(code)
  let output = true;
  expect(f()).toEqual(output);

  let output_ = $T.bool(output);
  let input_ = $T.num(-1);
  expect($I.evaluate($T.program_(), input_)).toEqual(output_);

  /*

  expect(received).toBe(expected) // Object.is equality

  Expected: {"kind": "boolean", "value": true}
  Received: {"kind": "boolean", "value": true}

  Difference:

  Compared values have no visual difference. Note that you are testing for equality with the stricter `toBe` matcher using `Object.is`. For deep equality only, use `toEqual` instead.

  */
})

test('boolean 2', () => {
  $T.clear();

  let code = `
  function F() {
    $T.return_($T.bool(false));
    return false;
  }
  F
  `

  let f = eval(code)
  let output = false;
  expect(f()).toEqual(output);

  let output_ = $T.bool(output);
  let input_ = $T.num(-1);
  expect($I.evaluate($T.program_(), input_)).toEqual(output_);
})

test('number', () => {
  $T.clear();

  let code = `
  function F() {
    $T.return_($T.num(36));
    return 36;
  }
  F
  `

  let f = eval(code)
  let output = 36;
  expect(f()).toEqual(output);

  let output_ = $T.num(output);
  let input_ = $T.num(-1);
  expect($I.evaluate($T.program_(), input_)).toEqual(output_);
})

test('number 2', () => {
  $T.clear();

  let code = `
  function F() {
    $T.return_($T.num(-36));
    return -36;
  }
  F
  `

  let f = eval(code)
  let output = -36;
  expect(f()).toEqual(output);

  let output_ = $T.num(output);
  let input_ = $T.num(-1);
  expect($I.evaluate($T.program_(), input_)).toEqual(output_);
})

test('string', () => {
  $T.clear();

  let code = `
  function F() {
    $T.return_($T.str('foo'));
    return 'foo';
  }
  F
  `

  let f = eval(code)
  let output = 'foo';
  expect(f()).toEqual(output);

  let output_ = $T.str(output);
  let input_ = $T.num(-1);
  expect($I.evaluate($T.program_(), input_)).toEqual(output_);
})

test('string 2', () => {
  $T.clear();

  let code = `
  function F() {
    $T.return_($T.str('bar'));
    return 'bar';
  }
  F
  `

  let f = eval(code)
  let output = 'bar';
  expect(f()).toEqual(output);

  let output_ = $T.str(output);
  let input_ = $T.num(-1);
  expect($I.evaluate($T.program_(), input_)).toEqual(output_);
})

test('binop', () => {
  $T.clear();

  let code = `
  function F() {
    $T.return_($T.lt($T.num(19), $T.num(20)));
    return 19 < 20;
  }
  F
  `

  let f = eval(code)
  let output = true;
  expect(f()).toEqual(output);

  let output_ = $T.bool(output);
  let input_ = $T.num(-1);
  expect($I.evaluate($T.program_(), input_)).toEqual(output_);
})

test('binop 2', () => {
  $T.clear();

  let code = `
  function F() {
    $T.return_($T.lt($T.num(21), $T.num(20)));
    return 21 < 20;
  }
  F
  `

  let f = eval(code)
  let output = false;
  expect(f()).toEqual(output);

  let output_ = $T.bool(output);
  let input_ = $T.num(-1);
  expect($I.evaluate($T.program_(), input_)).toEqual(output_);
})