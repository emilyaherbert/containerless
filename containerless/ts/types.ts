export type BinOp = '+';

type LetExp = { kind: 'let', name: string, named: Exp, body: Exp };
type IfExp = { kind: 'if', cond: Exp, truePart: Exp, falsePart: Exp };
// type CallbackExp = { kind: 'callback', event: string, arg: string, body: Exp };
export type Exp
    =  { kind: 'unknown' }
    | { kind: 'number', value: number }
    | { kind: 'identifier', name: string }
    | { kind: 'binop', op: BinOp, e1: Exp, e2: Exp }
    | IfExp
    | LetExp;

type Context
    = { kind: 'let-body', exp: LetExp }
    | { kind: 'if-true', exp: IfExp }
    | { kind: 'if-false', exp: IfExp }
    | { kind: 'empty', exp: Exp };

export function identifier(name: string): Exp {
    return { kind: 'identifier', name };
}

export function number(value: number): Exp {
    return { kind: 'number', value };
}

export function if_(cond: Exp, truePart: Exp, falsePart: Exp): IfExp {
    return { kind: 'if', cond, truePart, falsePart };
}

/**
 * A complete trace cannot be extended any further. A trace is only completed
 * when we reach the end of a callback function (defined in callbacks.ts).
 */
// type CompleteTrace = { kind: 'complete', trace: Exp };

// TODO(arjun): Think about how a callback can "capture" a part of the trace

type PartialTrace = {
    kind: 'partial',
    trace: Exp,
    cursor: Context
};

function mergeExp(e1: Exp, e2: Exp): Exp {
    if (e1.kind === 'unknown') {
        return e2;
    }
    else if (e2.kind === 'unknown') {
        return e1;
    }
    else if (e1.kind === 'number' && e2.kind === 'number') {
        if (e1.value !== e2.value) {
            throw new Error(`Cannot merge numbers ${e1.value} and ${e2.value}`);
        }
        return e1;
    }
    else if (e1.kind === 'identifier' && e2.kind === 'identifier') {
        if (e1.name !== e2.name) {
            throw new Error(`Cannot merge identifiers ${e1.name} and
                ${e2.name}`);
        }
        return e1;
    }
    else if (e1.kind === 'let' && e2.kind === 'let') {
        if (e1.name !== e2.name) {
            throw new Error(`Cannot merge let expressions naming ${e1.name}
                and ${e2.name}`);
        }
        e1.named = mergeExp(e1.named, e2.named);
        e1.body = mergeExp(e1.body, e2.body);
        return e1;
    }
    else if (e1.kind === 'if' && e2.kind === 'if') {
        e1.cond = mergeExp(e1.cond, e2.cond);
        e1.truePart = mergeExp(e1.truePart, e2.truePart);
        e1.falsePart = mergeExp(e1.falsePart, e2.falsePart);
        return e1;
    }
    else if (e1.kind === 'binop' && e2.kind === 'binop') {
        if (e1.op !== e2.op) {
            throw new Error(`Cannot merge operations ${e1.op} and ${e2.op}`);
        }
        e1.e1 = mergeExp(e1.e1, e1.e2);
        e1.e2 = mergeExp(e1.e2, e2.e2);
        return e1;
    }
    else {
        throw new Error(`Cannot merge expressions of kinds ${e1.kind} and
            ${e2.kind}`);
    }
}

function mergeIntoContext(context: Context, exp: Exp) {
    if (exp.kind === 'unknown') {
        return;
    }
    else if (context.kind === 'let-body' && exp.kind === 'let') {
        if (context.exp.name !== exp.name) {
            throw 'cannot merge';
        }
        context.exp.named = mergeExp(context.exp.named, exp.named);
        context.exp.body = mergeExp(context.exp.body, exp.body);
    }
    else if ((context.kind === 'if-true' || context.kind == 'if-false')
        && exp.kind === 'if') {
        context.exp.cond = mergeExp(context.exp.cond, exp.cond);
        context.exp.truePart = mergeExp(context.exp.truePart, exp.truePart);
        context.exp.falsePart = mergeExp(context.exp.falsePart, exp.falsePart);
    }
    else {
        throw new Error(`Cannot merge expression of type ${exp.kind} into
            context of type ${context.kind}`);
    }
}

export function append(trace: PartialTrace, context: Context) {
    if (trace.cursor.kind === 'empty') {
        mergeIntoContext(context, trace.trace);
        trace.cursor = context;
        trace.trace = context.exp;
    }
    else if (trace.cursor.kind === 'let-body') {
        mergeIntoContext(context, trace.cursor.exp.body);
        trace.cursor.exp.body = context.exp;
        trace.cursor = context;
    }
    else if (trace.cursor.kind === 'if-true') {
        mergeIntoContext(context, trace.cursor.exp.truePart);
        trace.cursor.exp.truePart = context.exp;
        trace.cursor = context;
    }
    else if (trace.cursor.kind === 'if-false') {
        mergeIntoContext(context, trace.cursor.exp.falsePart);
        trace.cursor.exp.falsePart = context.exp;
        trace.cursor = context;
    }
    else {
        throw 'unreachable';
    }
}


export function unknown(): Exp {
    return { kind: 'unknown' };
}
export function emptyPartialTrace(): PartialTrace {
    let exp = unknown();
    return {
        kind: 'partial',
        trace: exp,
        cursor: { kind: 'empty', exp }
    };
       
}

export function traceLet(trace: PartialTrace, name: string, named: Exp) {
    append(trace, {
        kind: 'let-body',
        exp: {
            kind: 'let',
            name: name,
            named: named,
            body: unknown()
        }
    });
}

export function traceIfTrue(trace: PartialTrace, cond: Exp) {
    append(trace, {
        kind: 'if-true',
        exp: if_(cond, unknown(), unknown())
    });
}

export function traceIfFalse(trace: PartialTrace, cond: Exp) {
    append(trace, {
        kind: 'if-false',
        exp: if_(cond, unknown(), unknown())
    });
}

export function traceComplete(trace: PartialTrace, exp: Exp) {
    let cursor = trace.cursor;
    if (cursor.kind === 'let-body') {
        cursor.exp.body = mergeExp(cursor.exp.body, exp);
    }
    else if (cursor.kind === 'if-false') {
        cursor.exp.falsePart = mergeExp(cursor.exp.falsePart, exp);
    }
    else if (cursor.kind === 'if-true') {
        cursor.exp.truePart =  mergeExp(cursor.exp.truePart, exp);
    }
    else {
        throw new Error('Not implemented');
    }
    trace.cursor = { kind: 'empty', exp: trace.trace };
}