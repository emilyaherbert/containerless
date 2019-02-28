import { Name, Id, Typ, Exp, Stmt, Obj } from "../ts/types";

class AST {
  constructor() {}

  private program : Stmt[] = [];
  private stack : Stmt[][] = [];
  private current = this.program;

  public push(e: Stmt) {
    this.current.push(e);
  }

  public pop() {
    this.current.pop();
  }

  public prev(): Stmt {
    if(this.current.length > 0) {
      return this.current[this.current.length - 1];
    } else {
      throw new Error("Expected nonempty this.current.");
    }
  }

  public push_scope() {
    this.stack.push(this.current);
    this.current = [];
  }

  public pop_scope() {
    this.current = this.stack.pop()!;
  }

  public get_current() {
    return this.current;
  }

  public get_program() {
    return this.program;
  }
}

let ast = new AST();
let ast_builder = new AST();
let args_stack : Exp[][] = [];

export function startTrace() {
  ast = new AST();
}

export function stopTrace() {
  ast_builder = ast;
  // Merge ast and ast_builder.
}

let nextName : Name = 0;
export function bind(e: Exp): Id {
    let name = nextName++;
    let t = getTyp(e);
    ast.push({ kind: 'let', name: name, e: e });
    return { kind: 'identifier', name: name, type: t };
}

export function update(e1: Exp, e2: Exp): void {
    ast.push({ kind: 'assignment', e1: e1, e2: e2 });
}

export function input(): Exp {
    return { kind: 'input' };
}

export function args(es: Exp[]) {
  args_stack.push(es);
}

export function params(): Id[] {
  if(args_stack.length > 0) {
    const es = args_stack[args_stack.length - 1];
    args_stack.pop();
    return es.map(bind);
  } else {
    throw new Error ("Found empty args_stack in params().");
  }
}

export function return_(value: Exp) {
  ast.push({ kind: 'return', value: value })
}

export function expectReturn(): Exp {
  const theReturn = ast.prev();
  ast.pop();
  if(theReturn.kind === 'return') {
    return bind(theReturn.value);
  } else {
    throw new Error("Expected kind return for theReturn.kind.");
  }
}

// If there is no else, initialize as empty block Exp.
export function if_(test: Exp) {
  ast.push({ kind: 'if',
      test: test,
      then: { kind: 'unknown' },
      else: { kind: 'block', body: []} });
}

export function ifElse(test: Exp) {
    ast.push({
        kind: 'if',
        test: test,
        then: { kind: 'unknown' },
        else: { kind: 'unknown' } });
}

export function enterIf(condition: boolean) {
    let theIf = ast.prev();
    if (theIf.kind !== 'if') {
        throw new Error("Expected previous Stmt to be an if.");
    }
    ast.push_scope();

    // TODO(arjun): Test condition
    if (condition) {
        theIf.then = { kind: 'block', body: ast.get_current() };
    }
    else {
        theIf.else = { kind: 'block', body: ast.get_current() };
    }
}

export function exitIf() {
    ast.pop_scope();
}

function getTyp(e: Exp): Typ {
    if (e.kind === 'string') {
        return { kind: 'string' };
    }
    else if (e.kind === 'boolean') {
        return { kind: 'boolean' };
    }
    else if (e.kind === 'number') {
        return { kind: 'number' };
    }
    else if (e.kind === 'identifier') {
        return e.type;
    }
    else if (e.kind === 'unaryop') {
        if (unaryOpReturnType.has(e.op)) {
            return unaryOpReturnType.get(e.op)!;
        }
        else {
            throw new Error("Found unimplemented unaryop.");
        }
    }
    else if (e.kind === 'binop') {
        if (binOpReturnType.has(e.op)) {
            return binOpReturnType.get(e.op)!;
        }
        else {
            throw new Error("Found unimplemented binop.");
        }
    }
    else if (e.kind === 'ternary') {
        const consequentType = getTyp(e.consequent);
        const alternateType = getTyp(e.alternate);
        if (consequentType.kind !== alternateType.kind) {
            // TODO(Chris): for now
            throw new Error(`Consequent and alternate have different types, ${consequentType.kind} and ${alternateType.kind}.`);
        } else {
            return consequentType;
        }
    }
    else if (e.kind === 'input') {
        return { kind: 'number' }; // TODO(arjun): For now
    }
    else if (e.kind === 'object') {
      return { kind: 'object' };
    }
    else {
      console.log(e);
      throw new Error('Not implemented.');
    }
}

const unaryOpReturnType = new Map<string, Typ>();
const binOpReturnType = new Map<string, Typ>();

function genericUnaryOp(opStr: string, inputType: Typ, returnType: Typ): (e: Exp) => Exp {
    unaryOpReturnType.set(opStr, returnType);
    return function(e: Exp): Exp {
        if (getTyp(e).kind === inputType.kind) {
            return { kind: 'unaryop', op: opStr, e };
        } else {
            throw new Error('Not implemented.');
        }
    }
}

function genericBinOp(opStr: string, inputType: Typ, returnType: Typ): (e1: Exp, e2: Exp) => Exp {
    binOpReturnType.set(opStr, returnType);
    return function(e1: Exp, e2: Exp): Exp {
        if (getTyp(e1).kind === inputType.kind &&
            getTyp(e2).kind === inputType.kind) {
            return { kind: 'binop', op: opStr, e1, e2 };
        } else {
            throw new Error('Not implemented.');
        }
    }
}

// TODO(Chris): Many of these operations were very haphazardly named, and the export names
// may need to change if we have separate exported functions like +num and +str, etc.
export const neg            = genericUnaryOp('unary-', { kind: 'number' } , { kind: 'number' });
export const plus           = genericUnaryOp('unary+', { kind: 'number' } , { kind: 'number' });
export const not            = genericUnaryOp('!',      { kind: 'boolean' }, { kind: 'boolean' });
export const bitnot         = genericUnaryOp('~',      { kind: 'number' } , { kind: 'number' });

// TODO(Chris): should we just cut to the chase and return "undefined" here?
export function _void(e: Exp): Exp {
  return { kind: 'unaryop', op: 'void', e };
}

export const eq             = genericBinOp('==num',  { kind: 'number' } , { kind: 'boolean' });
export const ineq           = genericBinOp('!=num',  { kind: 'number' } , { kind: 'boolean' });
export const exacteq        = genericBinOp('===num', { kind: 'number' } , { kind: 'boolean' });
export const exactineq      = genericBinOp('!==num', { kind: 'number' } , { kind: 'boolean' });
export const lt             = genericBinOp('<',      { kind: 'number' } , { kind: 'boolean' });
export const gt             = genericBinOp('>',      { kind: 'number' } , { kind: 'boolean' });
export const leq            = genericBinOp('<=',     { kind: 'number' } , { kind: 'boolean' });
export const geq            = genericBinOp('>=',     { kind: 'number' } , { kind: 'boolean' });
export const sub            = genericBinOp('-',      { kind: 'number' } , { kind: 'number' });
export const div            = genericBinOp('/',      { kind: 'number' } , { kind: 'number' });
export const mul            = genericBinOp('*',      { kind: 'number' } , { kind: 'number' });
export const remainder      = genericBinOp('%',      { kind: 'number' } , { kind: 'number' });
export const pow            = genericBinOp('**',     { kind: 'number' } , { kind: 'number' });
export const lshift         = genericBinOp('<<',     { kind: 'number' } , { kind: 'number' });
export const rshift         = genericBinOp('>>',     { kind: 'number' } , { kind: 'number' });
export const unsignedrshift = genericBinOp('>>>',    { kind: 'number' } , { kind: 'number' });
export const bitand         = genericBinOp('&',      { kind: 'number' } , { kind: 'number' });
export const bitor          = genericBinOp('|',      { kind: 'number' } , { kind: 'number' });
export const bitxor         = genericBinOp('^',      { kind: 'number' } , { kind: 'number' });
export const and            = genericBinOp('&&',     { kind: 'boolean' }, { kind: 'boolean' });
export const or             = genericBinOp('||',     { kind: 'boolean' }, { kind: 'boolean' });


binOpReturnType.set('+num', { kind: 'number' });
binOpReturnType.set('+str', { kind: 'string' });
export function add(e1: Exp, e2: Exp): Exp {
    if (getTyp(e1).kind === 'number' &&
        getTyp(e2).kind === 'number') {
        return { kind: 'binop', op: '+num', e1, e2 };
    } else if (getTyp(e1).kind === 'string' &&
               getTyp(e2).kind === 'string') {
        return { kind: 'binop', op: '+str', e1, e2 };
    } else {
        throw new Error('Not implemented.');
    }
}

export function ternary(test: Exp, consequent: Exp, alternate: Exp): Exp {
    if (getTyp(test).kind === 'boolean') {
        return { kind: 'ternary', test, consequent, alternate };
    } else {
        throw new Error(`Ternary expression test is ${getTyp(test).kind}, not a boolean`);
    }
}

export function num(n: number): Exp {
    return { kind: 'number', value: n };
}

export function str(s: string): Exp {
    return { kind: 'string', value: s };
}

export function bool(b: boolean): Exp {
  return { kind: 'boolean', value: b };
}

export function undefined(): Exp {
  return { kind: 'undefined' };
}

export function object(o: Obj): Exp {
  return { kind: 'object', value: o };
}

export function member(o: Exp, f: string): Exp {
  if(o.kind !== 'identifier') {
    console.log(o);
    throw new Error("Expected identifier.");
  }
  if(o.type.kind !== 'object') {
    throw new Error("Expected object.");
  }
  return { kind: 'member', object: o, field: f };
}

export function log() {
    console.log(JSON.stringify(ast.get_program(), null, 2));
}

// TODO(emily): Used for testing, replace with something more elegant.
// i.e. https://github.com/plasma-umass/ElementaryJS/blob/master/ts/runtime.ts#L316
export function clear() {
  ast = new AST();
  ast_builder = new AST();
  args_stack = [];
}

export function program_() {
  return ast.get_program();
}
