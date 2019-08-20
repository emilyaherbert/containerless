const $T = require('../dist/runtime');
const $R = require('../native');

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
  let tenv = $T.getTEnv();
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
  let tenv = $T.getTEnv();
  console.log(tenv);
  let program = $T.getProgram();
  let ast_outputs : Exp[] = [];
  for(let i=0; i<inputs.length; i++) {
    ast_outputs.push(interp.eval(program, wrap_exp(inputs[i])));
  }
  for(let i=0; i<inputs.length; i++) {
    expect(ast_outputs[i]).toEqual(wrap_exp(func_outputs[i]));
  }
}

export function run_linked_test(code: string, input: (string | boolean | number)) {
  $T.clear();
  let trace = insertTracing.transform(code);
  let func_output = eval(trace)(input);

  let classes = $T.getClasses();
  let tenv = $T.getTEnv();
  let program = $T.getProgram();
  let programStr = $T.getProgramAsString();
  //console.log(programStr);

  $R.compile(programStr);
  let rust_output = $R.run(input);
  //console.log(rust_output);

  expect(rust_output).toEqual(func_output);
}

export function run_linked_tests(code: string, inputs: (string | boolean | number)[]) {
  $T.clear();
  let trace = insertTracing.transform(code);
  let func_outputs : (string | boolean | number)[] = [];
  for(let i=0; i<inputs.length; i++) {
    func_outputs.push(eval(trace)(inputs[i]));
  }

  let classes = $T.getClasses();
  let tenv = $T.getTEnv();
  let program = $T.getProgram();
  let programStr = $T.getProgramAsString();
  //console.log(programStr);

  $R.compile(programStr);
  let rust_outputs : (string | boolean | number)[] = [];
  for(let i=0; i<inputs.length; i++) {
    rust_outputs.push($R.run(inputs[i]));
  }

  for(let i=0; i<inputs.length; i++) {
    expect(rust_outputs[i]).toEqual(func_outputs[i]);
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