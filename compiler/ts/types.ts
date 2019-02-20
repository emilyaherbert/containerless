export type Name = number;

export type Id = { kind: 'identifier', name: Name, type: Typ };

export type Typ =
    { kind: 'number' } |
    { kind: 'string' } |
    { kind: 'boolean' } |
    { kind: 'undefined' };

export type Exp =
    { kind: 'number', value: number } |
    { kind: 'string', value: string } |
    { kind: 'boolean', value: boolean } |
    Id |
    { kind: 'input' } |
    { kind: 'parameter' } |
    { kind: 'unaryop', op: string, e: Exp } |
    { kind: 'binop', op: string, e1: Exp, e2: Exp };

export type Stmt =
    { kind: 'let', name: Name, e: Exp } |
    { kind: 'assignment', id: Id, e: Exp } |
    { kind: 'if', test: Exp, then: Stmt, else: Stmt } |
    { kind: 'block', body: Stmt[] } |
    { kind: 'argument', e: Exp } |
    { kind: 'return', value: Exp } |
    { kind: 'unknown' }