
type Name = number;

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
    { kind: 'binop', op: string, e1: Exp, e2: Exp };

export type Stmt =
    { kind: 'let', name: Name, e: Exp } |
    { kind: 'block', body: Stmt[] };

let program : Stmt[] = [];
let current = program;

let nextName : Name = 0;
export function bind(e: Exp): Id {
    let name = nextName++;
    let t = getTyp(e);
    current.push({ kind: 'let', name: name, e: e });
    return { kind: 'identifier', name: name, type: t };
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
        if (e.op === '<') {
            return { kind: 'boolean' };
        }
        else if (e.op === '+num') {
            return { kind: 'number' };
        }
        else {
            throw 'Not implemented';
        }
    }
    else {
        throw 'Not implemented';
    }
}

export function lt(e1: Exp, e2: Exp): Exp {
    if (getTyp(e1).kind === 'number' &&
        getTyp(e2).kind === 'number') {
        return { kind: 'binop', op: '<', e1, e2 };
    }
    else {
        throw 'Not implemented';
    }
}

export function add(e1: Exp, e2: Exp): Exp {
    if (getTyp(e1).kind === 'number' &&
        getTyp(e2).kind === 'number') {
        return { kind: 'binop', op: '+num', e1, e2 };
    }
    else {
        throw 'Not implemented';
    }
}

export function num(n: number): Exp {
    return { kind: 'number', value: n };
}

export function str(s: string): Exp {
    return { kind: 'string', value: s };
}

export function log() {
    console.log(JSON.stringify(program, null, 2));

}