import { Name, Id, Typ, Exp, Stmt } from "../ts/types";

function pretty_print(obj: any) {
  console.log(JSON.stringify(obj, null, 2));
}

// TODO: Try/ catch ?
function unwrap_boolean(e: Exp): Boolean {
  switch(e.kind) {
    case 'boolean': return e.value;
    default: throw "Expected boolean in unwrap_boolean."
  }
}

function eval_binop(op: String, v1: Exp, v2: Exp): Exp {
  /*
  switch([v1.kind, v2.kind]) {
    case ['number', 'number']: {
      switch(op) {
        // Can't infer that v1.kind === 'number'?
        case '+': return { kind: 'number', value: (v1.value + v2.value)}
      }
    }
    case ['string', 'string']: {

    }
    case ['boolean', 'boolean']: {

    }
  }
  */

  if(v1.kind === 'number' && v2.kind === 'number') {
    switch(op) {
      case '+': return { kind: 'number', value: (v1.value + v2.value) };
      case '-': return { kind: 'number', value: (v1.value - v2.value) };
      case '*': return { kind: 'number', value: (v1.value * v2.value) };
      case '/': return { kind: 'number', value: (v1.value / v2.value) };
      case '<': return { kind: 'boolean', value: (v1.value < v2.value) };
      case '<=': return { kind: 'boolean', value: (v1.value <= v2.value) };
      case '>': return { kind: 'boolean', value: (v1.value > v2.value) };
      case '>=': return { kind: 'boolean', value: (v1.value >= v2.value) };
      case '==': return { kind: 'boolean', value: (v1.value == v2.value) };
      case '===': return { kind: 'boolean', value: (v1.value === v2.value) };
      case '!=': return { kind: 'boolean', value: (v1.value != v2.value) };
      case '!==': return { kind: 'boolean', value: (v1.value !== v2.value) };
      default: throw "Found unimplemented op in eval_binop."
    }
  } else if(v1.kind === 'boolean' && v2.kind === 'boolean') {
    switch(op) {
      case '&&': return { kind: 'boolean', value: (v1.value && v2.value) };
      case '||': return { kind: 'boolean', value: (v1.value || v2.value) };
      case '==': return { kind: 'boolean', value: (v1.value == v2.value) };
      case '===': return { kind: 'boolean', value: (v1.value === v2.value) };
      case '!=': return { kind: 'boolean', value: (v1.value != v2.value) };
      case '!==': return { kind: 'boolean', value: (v1.value !== v2.value) };
      default: throw "Found unimplemented op in eval_binop."
    }
  } else if(v1.kind === 'string' && v2.kind === 'string') {
    switch(op) {
      case '+': return { kind: 'string', value: (v1.value + v2.value) };
      case '==': return { kind: 'boolean', value: (v1.value == v2.value) };
      case '===': return { kind: 'boolean', value: (v1.value === v2.value) };
      case '!=': return { kind: 'boolean', value: (v1.value != v2.value) };
      case '!==': return { kind: 'boolean', value: (v1.value !== v2.value) };
      default: throw "Found unimplemented op in eval_binop."
    }
  } else {
    throw "Found mismatched v1.kind and v2.kind in eval_binop."
  }
}

function eval_exp(e: Exp, input: Exp): Exp {
  switch(e.kind) {
    case 'number': return e;
    case 'string': return e;
    case 'boolean': return e;
    case 'input': return input;
    case 'binop': {
      let v1 = eval_exp(e.e1, input);
      let v2 = eval_exp(e.e2, input);
      return eval_binop(e.op, v1, v2);
    }
    default: throw "Found unimplemented e.kind in eval_exp.";
  }
}

function eval_stmt(e: Stmt, input: Exp): Exp {
  switch(e.kind) {
    case 'let': throw "Found unimplemented let case in eval_stmt."
    case 'if': {
      let c = eval_exp(e.test, input);
      if(unwrap_boolean(c)) return eval_stmt(e.then, input);
      else return eval_stmt(e.else, input);
    }
    case 'block': throw "Found unimplemented block case in eval_stmt."
    case 'return': return eval_exp(e.value, input);
    case 'unknown': throw "Found unimplemented unknown case in eval_stmt."
    default: throw "Found unimplemented e.kind in eval_stmt.";
  }
}

export function evaluate(e: Stmt[], input: Exp): Exp {
  if(e.length === 0) {
    throw "No more Stmt's!"
  } else {
    return eval_stmt(e[0], input);
  }
}