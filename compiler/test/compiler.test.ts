import * as $T from '../ts/runtime';
import * as $I from '../ts/interpreter';

test('please work', () => {
  $T.clear();

  let code = `
  function F(x) {
      let $x = $T.input();
      $T.if_($T.lt($x, $T.num(20)));
      if (x < 20) {
          $T.enterIf(true);
          let $y = $T.bind($T.lt($x, $T.num(2)));
          let y = x < 2;
      } else {
          $T.enterIf(false);
      }
      $T.exitIf();
  }
  F`;

  let f = eval(code);
  f(100);
  //$T.log();
});

test('please work 2', () => {
  $T.clear();

  let code = `
  function F(x) {
      let $x = $T.input();
      $T.if_($T.lt($x, $T.num(20)));
      if (x < 20) {
          $T.enterIf(true);
          let $y = $T.bind($T.lt($x, $T.num(2)));
          let y = x < 2;
          return "foo";
      } else {
          $T.enterIf(false);
          return "bar";
      }
      $T.exitIf();
  }
  F`;

  let f = eval(code);
  expect(f(10)).toBe("foo");
});

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
  let output_ = $T.bool(output);

  expect(f()).toEqual(output);
  expect($I.evaluate($T.program_(), '')).toEqual(output_);

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
  expect($I.evaluate($T.program_(), '')).toEqual(output_);
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
  let output_ = $T.num(output);

  expect(f()).toEqual(output);
  expect($I.evaluate($T.program_(), '')).toEqual(output_);
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
  expect($I.evaluate($T.program_(), '')).toEqual(output_);
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
  let output_ = $T.str(output);

  expect(f()).toEqual(output);
  expect($I.evaluate($T.program_(), '')).toEqual(output_);
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
  let output_ = $T.str(output);

  expect(f()).toEqual(output);
  expect($I.evaluate($T.program_(), '')).toEqual(output_);
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
  let output_ = $T.bool(output);

  expect(f()).toEqual(output);
  expect($I.evaluate($T.program_(), '')).toEqual(output_);
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
  let output_ = $T.bool(output);

  expect(f()).toEqual(output);
  expect($I.evaluate($T.program_(), '')).toEqual(output_);
})

/*
test('please work 3', () => {
  $T.clear();

  let code = `
  function F(x) {
      let $x = $T.input();
      $T.if_($T.lt($T.num(21), $T.num(20)));
      if (21 < 20) {
          $T.enterIf(true);
          let $y = $T.bind($T.lt($x, $T.num(2)));
          let y = x < 2;
          $T.return_($T.str('foo'))
          return 'foo';
      } else {
          $T.enterIf(false);
          $T.return_($T.str('bar'))
          return 'bar';
      }
      $T.exitIf();
  }
  F`;

  let f = eval(code);
  let finput = 10;
  let foutput = 'bar';
  expect(f(finput)).toBe(foutput);

  let input = { kind: 'number', value: finput };
  let output = { kind: 'string', value: foutput };
  expect($I.evaluate($T.program_(), input)).toBe(output);
});
*/