export type BinOp = '+' | '-' | '>';

export type BlockExp = { kind: 'block', body: Exp[] };

type LetExp = { kind: 'let', name: string, named: Exp };
type SetExp = { kind: 'set', name: LVal, named: Exp };
type IfExp = { kind: 'if', cond: Exp, truePart: Exp[], falsePart: Exp[] };
type WhileExp = { kind: 'while', cond: Exp, body: Exp[] };

/**
 * event(eventArg, function(callbackArg) { body ... });
 */
type CallbackExp = {
    kind: 'callback',
    event: string,
    eventArg: Exp, // argument for the event, e.g. the URL to get
    callbackArgs: string[], // name of the arguments passed to the callback
    clos: Exp,
    body: Exp[] // body of the callback
};

type PrimAppExp = {
    kind: 'primApp',
    event: string,
    eventArgs: Exp[]
}

type LabelExp = { kind: 'label', name: string, body: Exp[] };
type BreakExp = { kind: 'break', name: string, value: Exp };

type IdExp = { kind: 'identifier', name: string };
type FromExp = { kind: 'from', exp: Exp, field: string };

type TEnv = { [key: string]: Exp };
type ObjExp = { kind: 'object', properties: TEnv };
type ClosExp = { kind: 'clos', tenv: TEnv };

type ArrayExp = { kind: 'array', exps: Exp[] };
type IndexExp = { kind: 'index', exp: Exp, i: Exp };

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
    | { kind: 'boolean', value: boolean }
    | IdExp
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
    | ObjExp
    | ClosExp
    | ArrayExp
    | IndexExp
    | PrimAppExp;

export type LVal = IdExp | FromExp | IndexExp

export const undefined_ : Exp = { kind: 'undefined' };

export function identifier(name: string): IdExp {
    return { kind: 'identifier', name };
}

export function number(value: number): Exp {
    return { kind: 'number', value };
}

export function boolean(value: boolean): Exp {
    return { kind: 'boolean', value: value };
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

export function callback(event: string, eventArg: Exp, callbackArgs: string[], clos: Exp, body: Exp[]): CallbackExp {
    return { kind: 'callback', event, eventArg, callbackArgs, clos, body };
}

export function let_(name: string, named: Exp): LetExp {
    return { kind: 'let', name, named };
}

export function set(name: LVal, named: Exp): SetExp {
    return { kind: 'set', name, named };
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

export function obj(properties: TEnv): ObjExp {
    return { kind: 'object', properties: properties };
}

export function array(exps: Exp[]): ArrayExp {
    return { kind: 'array', exps: exps };
}

export function from(exp: Exp, field: string): FromExp {
    return { kind: 'from', exp, field };
}

export function froms(clos: Exp, ids: string[]): FromExp[] {
    let ret: FromExp[] = [];
    for(let i = 0; i < ids.length; i++) {
        ret.push(from(clos, ids[i]));
    }
    return ret;
}

export function index(exp: Exp, i: Exp): IndexExp {
    return { kind: 'index', exp: exp, i: i };
}

export function primApp(event: string, eventArgs: Exp[]): PrimAppExp {
    return { kind: 'primApp', event: event, eventArgs: eventArgs };
}

export function clos(tenv: TEnv): ClosExp {
    return { kind: 'clos', tenv: tenv };
}

let nextId = 0;
export function* freshId(): IterableIterator<string> {
    while(true) {
        let ret = '$w' + nextId;
        nextId++;
        yield ret;
    }
}