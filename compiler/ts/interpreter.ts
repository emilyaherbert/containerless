import { Name, Id, Typ, Exp, Stmt } from "../ts/types";

class State {
  constructor() {}

  names = new Map();
  values : Exp[] = [];
}

export class Interpreter {
  constructor() {}

  private st = new State();
  private history : State[] = [];

  // Entry-point.
  public eval(e: Stmt[], input: Exp): Exp {
    this.st = new State();

    for(let i=0; i<e.length; i++) {
      this.eval_stmt(e[i], input);
    }

    if(this.st.values.length > 0) return this.st.values[this.st.values.length-1];
    else throw "No values!";
  }

  private eval_stmt(e: Stmt, input: Exp) {
    switch(e.kind) {
      case 'let': {
        let v = this.eval_exp(e.e, input);
        this.st.names.set(e.name, v);
        break;
      }
      case 'assignment': {
        let v = this.eval_exp(e.e, input);
        this.st.names.set(e.id.name, v);
        break;
      }
      case 'if': {
        let c = this.eval_exp(e.test, input);
        if(this.unwrap_boolean(c)) this.eval_stmt(e.then, input);
        else this.eval_stmt(e.else, input);
        break;
      }
      case 'block': {
        // potential errors here...
        for(let i=0; i<e.body.length; i++) {
          this.history.push(this.st);
          this.eval_stmt(e.body[i], input);
          this.st = this.history[this.history.length-1];
          this.history.pop();
        }
        break;
      }
      case 'return': {
        this.st.values.push(this.eval_exp(e.value, input));
        break;
      }
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
      case 'unaryop': {
        let v = this.eval_exp(e.e, input);
        return this.eval_unaryop(e.op, v);
      }
      case 'binop': {
        let v1 = this.eval_exp(e.e1, input);
        let v2 = this.eval_exp(e.e2, input);
        return this.eval_binop(e.op, v1, v2);
      }
      case 'identifier': {
        if(this.st.names.has(e.name)) {
          return this.st.names.get(e.name);
        } else {
          throw "Found free identifier."
        }
      }
      default: throw "Found unimplemented e.kind in eval_exp.";
    }
  }

  private eval_unaryop(op: String, v: Exp): Exp {
    if (v.kind === 'number') {
      switch (op) {
        case 'unary-': return { kind: 'number', value: -v.value };
        case 'unary+': return { kind: 'number', value: v.value };
        case '~': return { kind: 'number', value: ~v.value };
        default: throw "Found unimplemented op in eval_unaryop."
      }
    } else if (v.kind === 'boolean' && op === '!') {
      return { kind: 'boolean', value: !v.value };
    } else {
      throw "Found unimplemented op in eval_unaryop."
    }
  }

  // TODO(emily): Sync this with op's in runtime.ts.
  private eval_binop(op: String, v1: Exp, v2: Exp): Exp {
    if(v1.kind === 'number' && v2.kind === 'number') {
      switch(op) {
        case '+num': return { kind: 'number', value: (v1.value + v2.value) };
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

  // TODO(emily): Try/ catch ?
  private unwrap_boolean(e: Exp): boolean {
    switch(e.kind) {
      case 'boolean': return e.value;
      default: throw "Expected boolean in unwrap_boolean."
    }
  }

}
