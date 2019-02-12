import { Name, Id, Typ, Exp, Stmt } from "../ts/types";

let program : Stmt[] = [];
let stack: Stmt[][] = [];
let current = program;

let nextName : Name = 0;
export function bind(e: Exp): Id {
    let name = nextName++;
    let t = getTyp(e);
    console.log('Bind!!!');
    current.push({ kind: 'let', name: name, e: e });
    return { kind: 'identifier', name: name, type: t };
}

export function input(): Exp {
    return { kind: 'input' };
}

export function return_(value: Exp) {
    console.log('Return!!!');
  current.push({
    kind: 'return',
    value: value });
    console.log(program);
}

export function if_(test: Exp) {
    current.push({
        kind: 'if',
        test: test,
        then: { kind: 'unknown' },
        else: { kind: 'unknown' } });
}

export function enterIf(condition: boolean) {
    let theIf = current[current.length - 1];
    if (theIf.kind !== 'if') {
        throw 'Total disaster';
    }
    stack.push(current);
    current = [];
    if (condition) {
        theIf.then = { kind: 'block', body: current };
    }
    else {
        theIf.else = { kind: 'block', body: current };
    }
}

export function exitIf() {
    current = stack.pop()!;
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
    else if (e.kind === 'binop') {
        if (opReturnType.has(e.op)) {
            // TODO(Chriscbr): why do I need the ! operator when I am guarding the expression?
            return opReturnType.get(e.op)!;
        }
        else {
            throw 'Not implemented';
        }
    }
    else if (e.kind === 'input') {
        return { kind: 'number' } // TODO(arjun): For now
    }
    else {
        throw 'Not implemented';
    }
}

const opReturnType = new Map<string, Typ>();

function genericBinOp(opStr: string, inputType: Typ, returnType: Typ): (e1: Exp, e2: Exp) => Exp {
    opReturnType.set(opStr, returnType);
    return function(e1: Exp, e2: Exp): Exp {
        if (getTyp(e1).kind === inputType.kind &&
            getTyp(e2).kind === inputType.kind) {
            return { kind: 'binop', op: opStr, e1, e2 };
        } else {
            throw 'Not implemented';
        }
    }
}

export const lt             = genericBinOp('<',    { kind: 'number' } , { kind: 'boolean' });
export const gt             = genericBinOp('>',    { kind: 'number' } , { kind: 'boolean' });
export const leq            = genericBinOp('<=',   { kind: 'number' } , { kind: 'boolean' });
export const geq            = genericBinOp('>=',   { kind: 'number' } , { kind: 'boolean' });
export const add            = genericBinOp('+num', { kind: 'number' } , { kind: 'number' });
export const sub            = genericBinOp('-',    { kind: 'number' } , { kind: 'number' });
export const div            = genericBinOp('/',    { kind: 'number' } , { kind: 'number' });
export const mul            = genericBinOp('*',    { kind: 'number' } , { kind: 'number' });
export const remainder      = genericBinOp('%',    { kind: 'number' } , { kind: 'number' });
export const pow            = genericBinOp('**',   { kind: 'number' } , { kind: 'number' });
export const lshift         = genericBinOp('<<',   { kind: 'number' } , { kind: 'number' });
export const rshift         = genericBinOp('>>',   { kind: 'number' } , { kind: 'number' });
export const unsignedrshift = genericBinOp('>>>',  { kind: 'number' } , { kind: 'number' });
export const bitand         = genericBinOp('&',    { kind: 'number' } , { kind: 'number' });
export const bitor          = genericBinOp('|',    { kind: 'number' } , { kind: 'number' });
export const bitxor         = genericBinOp('^',    { kind: 'number' } , { kind: 'number' });
export const and            = genericBinOp('&&',   { kind: 'boolean' }, { kind: 'boolean' });
export const or             = genericBinOp('||',   { kind: 'boolean' }, { kind: 'boolean' });

export function num(n: number): Exp {
    return { kind: 'number', value: n };
}

export function str(s: string): Exp {
    return { kind: 'string', value: s };
}

export function bool(b: boolean): Exp {
  return { kind: 'boolean', value: b };
}

export function log() {
    console.log(JSON.stringify(program, null, 2));
}

// TODO: Used for testing, replace with something more elegant.
// i.e. https://github.com/plasma-umass/ElementaryJS/blob/master/ts/runtime.ts#L316
export function clear() {
  program = [];
  stack = [];
  current = program;
}

export function program_() {
  return program;
}
