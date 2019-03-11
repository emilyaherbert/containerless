const $T = require('../dist/runtime');
import { Interpreter } from '../ts/interpreter';
import * as insertTracing from '../ts/insertTracing';
import { Exp } from "../ts/types";

let interp = new Interpreter();

export function run_test(code: string, input: (string | boolean | number)) {
  $T.clear();
  // NOTE(arjun): We could also run 'eval(code)(input)', but we are not. We
  // are assuming that the tracing does not change the semantics of 'code'.
  let trace = insertTracing.transform(code);
  let func_output = eval(trace)(input);
  let classes = $T.getClasses();
  let program = $T.getProgram();
  let ast_output = interp.eval(program, wrap_exp(input));
  expect(ast_output).toEqual(wrap_exp(func_output));
}

export function run_tests(code: string, inputs: (string | boolean | number)[]) {
  $T.clear()
  let trace = insertTracing.transform(code);
  let func_outputs : (string | boolean | number)[] = [];
  for(let i=0; i<inputs.length; i++) {
    func_outputs.push(eval(trace)(inputs[i]));
  }
  let classes = $T.getClasses();
  let program = $T.getProgram();
  let ast_outputs : Exp[] = [];
  for(let i=0; i<inputs.length; i++) {
    ast_outputs.push(interp.eval(program, wrap_exp(inputs[i])));
  }
  for(let i=0; i<inputs.length; i++) {
    expect(ast_outputs[i]).toEqual(wrap_exp(func_outputs[i]));
  }
}

function wrap_exp(v : (number | string | boolean | undefined)): Exp {
  switch (typeof v) {
    case 'string': return $T.str(v);
    case 'number': return $T.num(v);
    case 'boolean': return $T.bool(v);
    case 'undefined': return $T.undefined_();
    default: throw "Found unexpected type in wrap_exp."
  }
}