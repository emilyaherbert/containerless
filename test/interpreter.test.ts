import * as $T from '../ts/runtime';
import { Interpreter } from '../ts/interpreter';

let interp = new Interpreter();

test('boolean', () => {
  $T.clear();

  let code = `
  $T.return_($T.bool(true));
  `

  eval(code);
  let input = $T.num(-1);
  let output = $T.bool(true);
  let result = interp.eval($T.getProgram(), input);
  
  expect(result).toEqual(output);
})

test('boolean 2', () => {
  $T.clear();

  let code = `
  $T.return_($T.bool(false));
  `

  eval(code);
  let input = $T.num(-1);
  let output = $T.bool(false);
  let result = interp.eval($T.getProgram(), input);
  
  expect(result).toEqual(output);
})

test('number', () => {
  $T.clear();

  let code = `
  $T.return_($T.num(36));
  `

  eval(code);
  let input = $T.num(-1);
  let output = $T.num(36);
  let result = interp.eval($T.getProgram(), input);
  
  expect(result).toEqual(output);
})

test('number 2', () => {
  $T.clear();

  let code = `
  $T.return_($T.num(-36));
  `

  eval(code);
  let input = $T.num(-1);
  let output = $T.num(-36);
  let result = interp.eval($T.getProgram(), input);
  
  expect(result).toEqual(output);
})

test('string', () => {
  $T.clear();

  let code = `
  $T.return_($T.str('foo'));
  `

  eval(code);
  let input = $T.num(-1);
  let output = $T.str('foo');
  let result = interp.eval($T.getProgram(), input);
  
  expect(result).toEqual(output);
})

test('string 2', () => {
  $T.clear();

  let code = `
  $T.return_($T.str('bar'));
  `

  eval(code);
  let input = $T.num(-1);
  let output = $T.str('bar');
  let result = interp.eval($T.getProgram(), input);
  
  expect(result).toEqual(output);
})

test('binop', () => {
  $T.clear();

  let code = `
  $T.return_($T.lt($T.num(19), $T.num(20)));
  `

  eval(code);
  let input = $T.num(-1);
  let output = $T.bool(true);
  let result = interp.eval($T.getProgram(), input);
  
  expect(result).toEqual(output);
})

test('binop 2', () => {
  $T.clear();

  let code = `
  $T.return_($T.lt($T.num(21), $T.num(20)));
  `

  eval(code);
  let input = $T.num(-1);
  let output = $T.bool(false);
  let result = interp.eval($T.getProgram(), input);
  
  expect(result).toEqual(output);
})

test('let', () => {
  $T.clear();

  let code = `
  let $y = $T.bind($T.num(2));
  $T.return_($y)
  `

  eval(code);
  let input = $T.num(-1);
  let output = $T.num(2);
  let result = interp.eval($T.getProgram(), input);
  
  expect(result).toEqual(output);
})

test('let 2', () => {
  $T.clear();

  let code = `
  let $x = $T.bind($T.num(2));
  let $y = $T.bind($T.lt($x,$T.num(5)));
  $T.return_($y)
  `

  eval(code);
  let input = $T.num(-1);
  let output = $T.bool(true);
  let result = interp.eval($T.getProgram(), input);
  
  expect(result).toEqual(output);
})

test('if', () => {
  $T.clear();

  let code = `
  $T.ifElse($T.bool(true));
  $T.enterIf(true);
  $T.return_($T.str('foo'));
  $T.exitIf();
  `

  eval(code);
  let input = $T.num(-1);
  let output = $T.str('foo');
  let result = interp.eval($T.getProgram(), input);
  
  expect(result).toEqual(output);
})

test('if 2', () => {
  $T.clear();

  let code = `
  $T.ifElse($T.bool(false));
  $T.enterIf(false);
  $T.return_($T.str('bar'));
  $T.exitIf();
  `

  eval(code);
  let input = $T.num(-1);
  let output = $T.str('bar');
  let result = interp.eval($T.getProgram(), input);
  
  expect(result).toEqual(output);
})

test('function', () => {
  $T.clear();

  let code = `
  function F(x) {
      let $x = $T.input();
      $T.ifElse($T.lt($x, $T.num(20)));
      $T.enterIf(true);
      $T.return_($T.str('foo'));
      $T.exitIf();
  }
  F`;

  eval(code)(10);
  let input = $T.num(10);
  let output = $T.str('foo');
  let result = interp.eval($T.getProgram(), input);
  
  expect(result).toEqual(output);
})

test('function 2', () => {
  $T.clear();

  let code = `
  function F(x) {
      let $x = $T.input();
      $T.ifElse($T.lt($x, $T.num(20)));
      $T.enterIf(false);
      $T.return_($T.str('bar'));
      $T.exitIf();
  }
  F`;

  eval(code)(100);
  let input = $T.num(100);
  let output = $T.str('bar');
  let result = interp.eval($T.getProgram(), input);
  
  expect(result).toEqual(output);
})

test('function 3', () => {
  $T.clear();

  let code = `
  function F(x) {
    let $x = $T.input();
    let $y = $T.bind($T.add($T.num(1),$T.num(1)));
    $T.return_($T.add($x,$y));
  }
  F
  `

  eval(code)(100);
  let input = $T.num(100);
  let output = $T.num(102);
  let result = interp.eval($T.getProgram(), input);
  
  expect(result).toEqual(output);
})