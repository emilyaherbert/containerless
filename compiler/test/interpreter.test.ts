import * as $T from '../ts/runtime';
import * as $I from '../ts/interpreter';

test('boolean', () => {
  $T.clear();

  let code = `
  $T.return_($T.bool(true));
  `

  eval(code);
  let input = $T.num(-1);
  let output = $T.bool(true);
  let result = $I.evaluate($T.program_(), input);
  
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
  let result = $I.evaluate($T.program_(), input);
  
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
  let result = $I.evaluate($T.program_(), input);
  
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
  let result = $I.evaluate($T.program_(), input);
  
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
  let result = $I.evaluate($T.program_(), input);
  
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
  let result = $I.evaluate($T.program_(), input);
  
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
  let result = $I.evaluate($T.program_(), input);
  
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
  let result = $I.evaluate($T.program_(), input);
  
  expect(result).toEqual(output);
})