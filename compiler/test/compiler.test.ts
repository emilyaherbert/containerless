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