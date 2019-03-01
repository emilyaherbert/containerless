import { Exp, Stmt, Obj } from "../ts/types";
import * as helpers from "./helpers";

class State {
  constructor() {}

  private names = new Map<number, Exp>([]);
  private stack : Map<number, Exp>[] = [];
  values : Exp[] = [];

  // Save the names that were declared in the current scope.
  // Start new scope.
  public push() {
    this.stack.push(new Map(this.names));
    this.names = new Map<number, Exp>([]);
  }

  // Set current scope to previous scope.
  // Throw out top scope.
  public pop() {
    if(this.stack.length > 0) {
      this.names = this.stack[this.stack.length - 1];
    }
    this.stack.pop();
  }

  // Check if declared in current scope.
  // Traverse through previous scopes from closest to furthest.
  public has(id: number): boolean {
    if(this.names.has(id)) {
      return true;
    }
    for(let i=this.stack.length-1; i>(-1); i--) {
      if(this.stack[i].has(id)) {
        return true;
      }
    }
    return false;
  }

  // Add to current scope.
  public set(id: number, e: Exp) {
    this.names.set(id, e);
  }

  // Check if declared in current scope.
  // Traverse through previous scopes from closest to furthest.
  public get(id: number): Exp | undefined {
    if(this.names.has(id)) {
      return this.names.get(id);
    }
    for(let i=this.stack.length-1; i>(-1); i--) {
      if(this.stack[i].has(id)) {
        return this.stack[i].get(id);
      }
    }
  }

  // Update most recent let from closest scope.
  public update(id: number, e: Exp) {
    if(this.names.has(id)) {
      this.names.set(id, e);
      return;
    }
    for(let i=this.stack.length-1; i>(-1); i--) {
      if(this.stack[i].has(id)) {
        this.stack[i].set(id, e);
        return;
      }
    }
  }
}

export class Interpreter {
  constructor() {}

  private st = new State();

  // Entry-point.
  public eval(e: Stmt[], input: Exp): Exp {
    this.st = new State();

    for(let i=0; i<e.length; i++) {
      this.eval_stmt(e[i], input);
    }

    if(this.st.values.length > 0) return this.st.values[this.st.values.length-1];
    else throw new Error("No values!");
  }

  private eval_stmt(e: Stmt, input: Exp) {
    switch(e.kind) {
      case 'let': {
        let v = this.eval_exp(e.e, input);
        this.st.set(e.name, v);
        break;
      }
      case 'assignment': {
        // TODO(Chris): this should be refactored
        if(e.e1.kind === 'identifier') {
          this.st.update(e.e1.name, this.eval_exp(e.e2, input));
        } else if (e.e1.kind === 'member') {
          // TODO(Chris): not sure if some kind of st.update is needed?
          // modifying the object should affect it for all references, since
          // we are not creating a copy
          let o = this.eval_exp(e.e1.object, input);
          helpers.unwrap_object(o)[e.e1.field] = this.eval_exp(e.e2, input);
        } else {
          throw new Error('Invalid assignment expression.');
        }
        break;
      }
      case 'if': {
        let c = this.eval_exp(e.test, input);
        if(helpers.unwrap_boolean(c)) this.eval_stmt(e.then, input);
        else this.eval_stmt(e.else, input);
        break;
      }
      case 'while': {
        while(helpers.unwrap_boolean(this.eval_exp(e.test, input))) {
          this.eval_stmt(e.body, input);
        }
        break;
      }
      case 'block': {
        this.st.push();
        for(let i=0; i<e.body.length; i++) {
          this.eval_stmt(e.body[i], input);
        }
        this.st.pop();
        break;
      }
      case 'return': {
        let v = this.eval_exp(e.value, input);
        this.st.values.push(v);
        break;
      }
      case 'unknown': throw new Error("Found unimplemented unknown case in eval_stmt.");
      default: throw new Error(`Found unimplemented kind ${e.kind} in eval_stmt.`);
    }
  }

  private eval_exp(e: Exp, input: Exp): Exp {
    switch(e.kind) {
      case 'number': return e;
      case 'string': return e;
      case 'boolean': return e;
      case 'undefined': return e;
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
      case 'ternary': {
        let test = this.eval_exp(e.test, input);
        if (test.kind !== 'boolean') {
          throw new Error("ternary test did not evaluate to a boolean.");
        }
        if (helpers.unwrap_boolean(test)) {
          return this.eval_exp(e.consequent, input);
        } else {
          return this.eval_exp(e.alternate, input);
        }
      }
      case 'identifier': {
        if(this.st.has(e.name)) {
          let v = this.st.get(e.name);
          if(typeof(v) !== 'undefined') {
            return v;
          } else {
            throw new Error("Found undefined identifier.");
          }
        } else {
          throw new Error("Found free identifier.");
        }
      }
      case 'object': {
        return e;
      }
      case 'member': {
        let o = this.eval_exp(e.object, input);
        let member = helpers.unwrap_object(o)[e.field];
        return this.eval_exp(member, input);
      }
      default: throw new Error(`Found unimplemented e.kind in eval_exp.`);
    }
  }

  private eval_unaryop(op: String, v: Exp): Exp {
    if (op === 'void') return { kind: 'undefined' };
    if (v.kind === 'number') {
      switch (op) {
        case 'unary-': return { kind: 'number', value: -v.value };
        case 'unary+': return { kind: 'number', value: v.value };
        case '~': return { kind: 'number', value: ~v.value };
        default: throw new Error("Found unimplemented op in eval_unaryop.");
      }
    } else if (v.kind === 'boolean' && op === '!') {
      return { kind: 'boolean', value: !v.value };
    } else {
      throw new Error("Found unimplemented op in eval_unaryop.");
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
        case '===num': return { kind: 'boolean', value: (v1.value === v2.value) };
        case '!=': return { kind: 'boolean', value: (v1.value != v2.value) };
        case '!==': return { kind: 'boolean', value: (v1.value !== v2.value) };
        default: throw new Error(`Found unimplemented ${op} in eval_binop.`);
      }
    } else if(v1.kind === 'boolean' && v2.kind === 'boolean') {
      switch(op) {
        case '&&': return { kind: 'boolean', value: (v1.value && v2.value) };
        case '||': return { kind: 'boolean', value: (v1.value || v2.value) };
        case '==': return { kind: 'boolean', value: (v1.value == v2.value) };
        case '===': return { kind: 'boolean', value: (v1.value === v2.value) };
        case '!=': return { kind: 'boolean', value: (v1.value != v2.value) };
        case '!==': return { kind: 'boolean', value: (v1.value !== v2.value) };
        default: throw new Error(`Found unimplemented ${op} in eval_binop.`);
      }
    } else if(v1.kind === 'string' && v2.kind === 'string') {
      switch(op) {
        case '+str': return { kind: 'string', value: (v1.value + v2.value) };
        case '==': return { kind: 'boolean', value: (v1.value == v2.value) };
        case '===': return { kind: 'boolean', value: (v1.value === v2.value) };
        case '!=': return { kind: 'boolean', value: (v1.value != v2.value) };
        case '!==': return { kind: 'boolean', value: (v1.value !== v2.value) };
        default: throw new Error(`Found unimplemented ${op} in eval_binop.`);
      }
    } else {
      throw new Error("Found mismatched v1.kind and v2.kind in eval_binop.");
    }
  }
}
