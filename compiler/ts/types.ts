
/*

Runtime Types

*/

export type Name = number;

export type Id = { kind: 'identifier', name: Name, type: Typ };

export type Typ =
    { kind: 'number' } |
    { kind: 'string' } |
    { kind: 'boolean' } |
    { kind: 'object' } |
    { kind: 'undefined' };

export type Object = { kind: 'object', value: { [key: string]: Exp } };

export type Exp =
    { kind: 'number', value: number } |
    { kind: 'string', value: string } |
    { kind: 'boolean', value: boolean } |
    { kind: 'undefined' } |
    Id |
    { kind: 'input' } |
    { kind: 'unaryop', op: string, e: Exp } |
    { kind: 'binop', op: string, e1: Exp, e2: Exp } |
    { kind: 'ternary', test: Exp, consequent: Exp, alternate: Exp } |
    Object |
    { kind: 'member', object: Exp, field: string };

export type If = { kind: 'if', test: Exp, then: Stmt, else: Stmt };
export type While = { kind: 'while', test: Exp, body: Stmt };
export type Return = { kind: 'return', value: Exp };

export type Stmt =
    { kind: 'let', name: Name, e: Exp } |
    { kind: 'assignment', e1: Exp, e2: Exp } |
    If |
    While |
    { kind: 'block', body: Stmt[] } |
    { kind: 'argument', e: Exp } |
    Return |
    { kind: 'unknown' }


/*

Internal Types

*/

export const unaryOpReturnType = new Map<string, Typ>();
export const binOpReturnType = new Map<string, Typ>();

export type Obj = { [key: string]: Exp };