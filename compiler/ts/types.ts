
/*

Runtime Types

*/

export type ObjectTyp = { kind: 'object', class: number };

export type Typ =
    { kind: 'number' } |
    { kind: 'string' } |
    { kind: 'boolean' } |
    ObjectTyp |
    { kind: 'undefined' };

export type Name = number;
export type IdExp = { kind: 'identifier', name: Name, type: Typ };
export type ObjectExp = { kind: 'object', class: number, value: { [key: string]: Exp } };

export type Exp =
    { kind: 'number', value: number } |
    { kind: 'string', value: string } |
    { kind: 'boolean', value: boolean } |
    { kind: 'undefined' } |
    IdExp |
    { kind: 'input' } |
    { kind: 'unaryop', op: string, e: Exp } |
    { kind: 'binop', op: string, e1: Exp, e2: Exp } |
    { kind: 'ternary', test: Exp, consequent: Exp, alternate: Exp } |
    ObjectExp |
    { kind: 'member', object: IdExp, field: string } ;

export type IfStmt = { kind: 'if', test: Exp, then: Stmt, else: Stmt };
export type WhileStmt = { kind: 'while', test: Exp, body: Stmt };
export type ReturnStmt = { kind: 'return', value: Exp };

export type Class = { kind: 'class', name: number, types: { [key: string]: Typ } , transitions : { [key: string]: number } };

export type Stmt =
    { kind: 'createClass', class: Class } |
    { kind: 'let', name: Name, e: Exp } |
    { kind: 'assignment', e1: Exp, e2: Exp } |
    IfStmt |
    WhileStmt |
    { kind: 'block', body: Stmt[] } |
    { kind: 'argument', e: Exp } |
    ReturnStmt |
    { kind: 'unknown' }