/**
 * This module is a runtime system for generating execution traces. The entry
 * point of this module is the 'newTrace' function, which creates an empty
 * trace. That function returns an object with several methods -- all prefixed
 * with the token 'trace' -- for incrementally building execution traces.
 *
 * A few things to note:
 *
 * - The 'traceIfTrue' and 'traceIfFalse' methods both trace an 'if' expression,
 *   and automatically enter the block for the true part and false part
 *   respectively.
 * - When control reaches the end of a block, the program must invoke the
 *   'exitBlock' method. The top-level of the program is also a block, therefore
 *   the program must invoke 'exitBlock' after tracing the last expression of
 *   the program.
 * - The 'traceCallback' method returns a new 'Trace' class to trace within the
 *   body of a callback function. The program must call 'exitBlock' at the end
 *   of a callback, since it is a block as well.
 */
export type BinOp = '+' | '-' | '>';

export type BlockExp = { kind: 'block', body: Exp[] };

export type LetExp = { kind: 'let', name: string, named: Exp };
export type SetExp = { kind: 'set', name: IdPath, named: Exp };
export type IfExp = { kind: 'if', cond: Exp, truePart: Exp[], falsePart: Exp[] };
export type WhileExp = { kind: 'while', cond: Exp, body: Exp[] };

/**
 * event(eventArg, function(callbackArg) { body ... });
 */
type CallbackExp = {
    kind: 'callback',
    event: string,
    eventArg: Exp, // argument for the event, e.g. the URL to get
    callbackArg: string, // name of the argument passed to the callback
    body: Exp[] // body of the callback
};
export type LabelExp = { kind: 'label', name: string, body: Exp[] };
export type BreakExp = { kind: 'break', name: string, value: Exp };

export type IdPath = string[];
export type TEnv = Map<string, IdPath>;
export type ClosExp = { kind: 'clos', tenv: TEnv };
export type FromExp = { kind: 'from', idPath: IdPath };

export type ArrayExp = { kind: 'array', exps: Exp[] };

/**
 * NOTE(arjun): We do not make a distinction between statements and expressions.
 * However, we do use blocks instead of deeply nesting let expressions. We do
 * this for two reasons:
 *
 * 1. It simplifies the definition of contexts significantly, and
 * 2. very deeply nested let expressions can lead to stack overflow errors,
 *    e.g., during serialization. I am not certain that serde will suffer this
 *    problem, but it is a problem I've encountered with other serialization
 *    libraries.
 *
 * We rely on some invariants for blocks to make sense:
 * 1. An unknown can only appear as the last expression in a block.
 */
export type Exp
    =  { kind: 'unknown' }
    | { kind: 'number', value: number }
    | { kind: 'identifier', name: string }
    | FromExp
    | { kind: 'string', value: string }
    | { kind: 'undefined' }
    | { kind: 'binop', op: BinOp, e1: Exp, e2: Exp }
    | IfExp
    | WhileExp
    | LetExp
    | SetExp
    | BlockExp
    | CallbackExp
    | LabelExp
    | BreakExp
    | ClosExp
    | ArrayExp;

export const undefined_ : Exp = { kind: 'undefined' };

export function identifier(name: string): Exp {
    return { kind: 'identifier', name };
}

export function number(value: number): Exp {
    return { kind: 'number', value };
}

export function string(value: string): Exp {
    return { kind: 'string', value };
}

export function binop(op: BinOp, e1: Exp, e2: Exp): Exp {
    return { kind: 'binop', op, e1, e2 };
}

export function if_(cond: Exp, truePart: Exp[], falsePart: Exp[]): IfExp {
    return { kind: 'if', cond, truePart, falsePart };
}

export function while_(cond: Exp, body: Exp[]): WhileExp {
    return { kind: 'while', cond: cond, body: body };
}

export function callback(event: string, eventArg: Exp, callbackArg: string, body: Exp[]): CallbackExp {
    return { kind: 'callback', event, eventArg, callbackArg, body };
}

export function let_(name: string, named: Exp): LetExp {
    return { kind: 'let', name, named };
}

export function set_(name: string, named: Exp): SetExp {
    return { kind: 'set', name: [name], named: named };
}

export function setPath(name: IdPath, named: Exp): SetExp {
    return { kind: 'set', name: name, named: named };
}

export function block(body: Exp[]): BlockExp {
    return { kind: 'block', body };
}

export function unknown(): Exp {
    return { kind: 'unknown' };
}

export function label(name: string, body: Exp[]): LabelExp {
    return { kind: 'label', name: name, body: body };
}

export function break_(name: string, value: Exp): BreakExp {
    return { kind: 'break', name: name, value };
}

export function clos(tenv: TEnv): ClosExp {
    return { kind: 'clos', tenv: tenv };
}

export function from(idPath: IdPath): FromExp {
    return { kind: 'from', idPath: idPath };
}

export function froms(clos: string, ids: string[]): FromExp[] {
    let ret: FromExp[] = [];
    for(let i=0; i<ids.length; i++) {
        ret.push(from([clos, ids[i]]));
    }
    return ret;
}

let nextId = 0;
export function* freshId(): IterableIterator<string> {
    while(true) {
        let ret = '$w' + nextId;
        nextId++;
        yield ret;
    }
}