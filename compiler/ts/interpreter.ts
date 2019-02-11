import { Name, Id, Typ, Exp, Stmt } from "../ts/types";

export class Interpreter {
  constructor() {}

  // entry-point
  public eval(e: Stmt[], input: Exp): Exp {
    if(e.length === 0) {
      throw "No more Stmt's!"
    } else {
      return this.eval_stmt(e[0], input);
    }
  }

  private eval_stmt(e: Stmt, input: Exp): Exp {
    switch(e.kind) {
      case 'let': throw "Found unimplemented let case in eval_stmt."
      case 'if': {
        let c = this.eval_exp(e.test, input);
        if(this.unwrap_boolean(c)) return this.eval_stmt(e.then, input);
        else return this.eval_stmt(e.else, input);
      }
      case 'block': throw "Found unimplemented block case in eval_stmt."
      case 'return': return this.eval_exp(e.value, input);
      case 'unknown': throw "Found unimplemented unknown case in eval_stmt."
      default: throw "Found unimplemented e.kind in eval_stmt.";
    }
  }

  private eval_exp(e: Exp, input: Exp): Exp {
    switch(e.kind) {
      case 'number': return e;
      case 'string': return e;
      case 'boolean': return e;
      case 'input': return input;
      case 'binop': {
        let v1 = this.eval_exp(e.e1, input);
        let v2 = this.eval_exp(e.e2, input);
        return this.eval_binop(e.op, v1, v2);
      }
      default: throw "Found unimplemented e.kind in eval_exp.";
    }
  }

  private eval_binop(op: String, v1: Exp, v2: Exp): Exp {
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

  // TODO: Try/ catch ?
  private unwrap_boolean(e: Exp): Boolean {
    switch(e.kind) {
      case 'boolean': return e.value;
      default: throw "Expected boolean in unwrap_boolean."
    }
  }

}