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

type IdPath = string[];
type TEnv = Map<string, IdPath>;

type BlockExp = { kind: 'block', body: Exp[] };

type LetExp = { kind: 'let', name: string, named: Exp };
type SetExp = { kind: 'set', name: string, named: Exp };
type IfExp = { kind: 'if', cond: Exp, truePart: Exp[], falsePart: Exp[] };
type WhileExp = { kind: 'while', cond: Exp, body: Exp[] };

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
type LabelExp = { kind: 'label', name: string, body: Exp[] };
type BreakExp = { kind: 'break', name: string, value: Exp };

type ClosExp = { kind: 'clos', tenv: TEnv };

type FromExp = { kind: 'from', idPath: IdPath };

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
    | ClosExp;

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

export function clos(tenv: TEnv): ClosExp {
    return { kind: 'clos', tenv: tenv };
}

export function from(idPath: IdPath): FromExp {
    return { kind: 'from', idPath: idPath };
}

let nextId = 0;
function* freshId(): IterableIterator<string> {
    while(true) {
        let ret = '$w' + nextId;
        nextId++;
        yield ret;
    }
}

type Cursor = { body: Exp[], index: number };

export class Trace {
    private trace: BlockExp;
    private cursorStack: Cursor[];
    private cursor: Cursor | undefined;
    private traceStack: Exp[];
    private env: TEnv;
    private envStack: TEnv[];


    constructor(body: Exp[]) {
        let exp = block(body);
        this.trace = exp;
        this.cursor = { body: exp.body, index: 0 };
        this.cursorStack = [];
        this.traceStack = [];
        this.env = new Map();
        this.envStack = [];
    }

    private getValidCursor(): Cursor {
        if (this.cursor === undefined) {
            throw new Error('Trace is complete');
        }
        return this.cursor;
    }

    private getCurrentExp() {
        let cursor = this.getValidCursor();
        if (cursor.index === cursor.body.length) {
            throw new Error(`Attempting to trace after the end of a block
                (index is ${cursor.index})`);
        }
        return cursor.body[cursor.index];
    }

    private getPrevExp() {
        let cursor = this.getValidCursor();
        if (cursor.index == 0) {
            throw new Error(`No previous exp.`);
        }
        return cursor.body[cursor.index - 1];
    }

    private mayIncrementCursor() {
        let cursor = this.getValidCursor();
        if (cursor.index === cursor.body.length - 1 &&
            cursor.body[cursor.index].kind !== 'unknown') {
            // At the end of the block during a re-trace.
            return;
        }
        cursor.index = cursor.index + 1;
    }

    private resetCursor() {
        // loops over current block
        let cursor = this.getValidCursor();
        cursor.index = 0;
    }

    private setExp(exp: Exp) {
        let cursor = this.getValidCursor();
        let kind = cursor.body[cursor.index].kind;
        if (kind !== 'unknown') {
            throw new Error(`Cannot discard expression of kind ${kind}`);
        }
        // Assumes that we are at the end of the block
        cursor.body[cursor.index] = exp;
        cursor.body.push(unknown());
        cursor.index = cursor.index + 1;
    }

    private enterBlock(cursor: Cursor) {
        if (this.cursor !== undefined) {
            this.cursorStack.push(this.cursor);
        }
        this.cursor = cursor;

        this.envStack.push(new Map(this.env));
    }

    pushArg(e: Exp) {
        this.traceStack.push(e);
    }

    popArg(): Exp {
        return this.traceStack.pop()!;
    }

    getTrace(): Exp {
        return this.trace;
    }

    newTrace() {
        this.cursor = { body: this.trace.body, index: 0 };
        this.cursorStack = [];
    }

    exitBlock() {
        if (this.cursor === undefined) {
            throw new Error(`called exitBlock on a complete trace`);
        }
        let cursor = this.cursor;
        if (cursor.index !== cursor.body.length - 1) {
            console.log(cursor);
            throw new Error('Exiting block too early');
        }
        if (this.cursor.body[cursor.index].kind === 'unknown') {
            this.cursor.body.pop();
        }
        this.cursor = this.cursorStack.pop();
        this.env = new Map(this.envStack.pop()!);
    }

    private quietExitBlock() {
        if (this.cursor === undefined) {
            throw new Error(`called exitBlock on a complete trace`);
        }
        this.cursor = this.cursorStack.pop();
        this.env = new Map(this.envStack.pop()!);
    }

    traceIdentifier(name: string): Exp  {
        if(!this.env.has(name)) {
            //console.log(name);
            //return identifier(name)
            throw new Error("Name not found!");
        } else {
            return from((this.env.get(name)!).slice(0));
        };
    }

    /**
     * Creates the trace expression 'let name = { unknown };' and enters the
     * block containing the 'unknown'.
     */
    traceNamed(name: string): void {
        this.env.set(name, [name]);

        let exp = this.getCurrentExp();
        if (exp.kind === 'unknown') {
            let namedBlock = block([unknown()]);
            this.setExp(let_(name, namedBlock));
            this.enterBlock({ body: namedBlock.body, index: 0 });
        }
        else if (exp.kind === 'let') {
            if (exp.name !== name) {
                throw new Error(`Cannot merge let with name ${name} into let with
                    name ${exp.name}`);
            }
            this.mayIncrementCursor();
            let named = exp.named;
            if (named.kind !== 'block') {
                throw new Error('Expected block on RHS of let');
            }
            this.enterBlock({ body: named.body, index: 0 });
        }
        else {
            throw new Error(`expected let, got ${exp.kind}`);
        }
    }

    traceLet(name: string, named: Exp): void {
        this.env.set(name, [name]);

        let exp = this.getCurrentExp();
        if (exp.kind === 'unknown') {
            this.setExp(let_(name, named));
        }
        else if (exp.kind === 'let') {
            if (exp.name !== name) {
                throw new Error(`Cannot merge let with name ${name} into let with
                    name ${exp.name}`);
            }
            exp.named = mergeExp(exp.named, named);
            this.mayIncrementCursor();
        }
        else {
            throw new Error(`expected let, got ${exp.kind}`);
        }
    }

    /*

        This is a special t.traceLet that constructs a closure for 'named'.
        It adds that closure as an Exp to the trace and returns it to be used
        in the JS code.

    */
    traceClos(name: string): ClosExp {
        let named = clos(new Map(this.env));
        console.log(named);
        this.traceLet(name, named);
        return named;
    }

    traceFunctionCall(name: string, call: string, theArgs: Exp[]): void {
        this.traceNamed(name);
        
        // NOTE: this may be backwards.
        for(let i=0; i<theArgs.length; i++) {
            this.pushArg(theArgs[i]);
        }

        // TODO: tricky tricky, think about if this will work every time.
        let w = freshId().next().value;
        this.pushArg(identifier(w));
        this.traceLet(w, identifier(call));
    }

    /*

        1. wrap the body of the function in a label $return
        2. bind the function name to [ TODO ]
        3. create fresh trace environment, bind y -> w.y and x -> x
        4. bind arguments to elems off of argument stack

    */
    traceFunctionBody(theArgs: string[], clos: ClosExp): void {
        this.traceLabel('$return');
        
        const w = this.popArg();
        if(w.kind !== 'identifier') {
            throw new Error("Expected identifier w!");
        }

        const tenv = new Map(clos.tenv);
        tenv.forEach(value => {
            // NOTE: may have to do deep copy here rather than in t.traceIdentifier
            if(value.length > 1) {
                value.shift();
            }
            value.unshift(w.name);
        })
        this.env = new Map(tenv);

        theArgs.forEach(theArg => {
            const tmp = this.popArg();
            if(tmp === undefined) {
                throw new Error("Found undefined argument!");
            }
            this.traceLet(theArg, tmp)
        })
    }

    traceSet(name: string, named: Exp): void {
        let exp = this.getCurrentExp();
        if (exp.kind === 'unknown') {
            this.setExp(set_(name, named));
        }
        else if (exp.kind === 'set') {
            if (exp.name !== name) {
                throw new Error(`Cannot merge set with name ${name} into set with
                    name ${exp.name}`);
            }
            exp.named = mergeExp(exp.named, named);
            this.mayIncrementCursor();
        }
        else {
            throw new Error(`expected set, got ${exp.kind}`);
        }
    }

    traceIfTrue(cond: Exp) {
        let exp = this.getCurrentExp();
        if (exp.kind === 'unknown') {
            let newBlock = [unknown()];
            this.setExp(if_(cond, newBlock, [unknown()]));
            this.enterBlock({ body: newBlock, index: 0 });
        }
        else if (exp.kind === 'if') {
            exp.cond = mergeExp(exp.cond, cond);
            this.mayIncrementCursor();
            this.enterBlock({ body: exp.truePart, index: 0 });
        }
        else {
            throw new Error(`expected if, got ${exp.kind}`);
        }
    }

    traceIfFalse(cond: Exp) {
        let exp = this.getCurrentExp();
        if (exp.kind === 'unknown') {
            let newBlock = [unknown()];
            this.setExp(if_(cond, [unknown()], newBlock));
            this.enterBlock({ body: newBlock, index: 0 });
        }
        else if (exp.kind === 'if') {
            exp.cond = mergeExp(exp.cond, cond);
            this.mayIncrementCursor();
            this.enterBlock({ body: exp.falsePart, index: 0 });
        }
        else {
            throw new Error(`expected if, got ${exp.kind}`);
        }
    }

    traceWhile(cond: Exp) {
        let exp = this.getCurrentExp();
        if (exp.kind === 'unknown') {
            let newBlock = [unknown()];
            this.setExp(while_(cond, newBlock));
            this.enterBlock({ body: newBlock, index: 0 });
        }
        else if (exp.kind === 'while') {
            exp.cond = mergeExp(exp.cond, cond);
            this.mayIncrementCursor();
            this.enterBlock({ body: exp.body, index: 0 });
        }
        else {
            throw new Error(`expected while, got ${exp.kind}`);
        }
    }

    traceLoop() {
        this.resetCursor();
    }

    traceCallback(event: string, eventArg: Exp, callbackArg: string): Trace {
        let exp = this.getCurrentExp();
        if (exp.kind === 'unknown') {
            let callbackBody: Exp[] = [ unknown() ];
            this.setExp(callback(event, eventArg, callbackArg, callbackBody));
            return new Trace(callbackBody);
        }
        else if (exp.kind === 'callback') {
            if (exp.event !== event || exp.callbackArg !== callbackArg) {
                throw new Error(`called traceCallback(${event}), but
                   hole contains traceCallback(${exp.event})`);
            }
            exp.eventArg = mergeExp(exp.eventArg, eventArg);

            this.mayIncrementCursor();
            return new Trace(exp.body);
        }
        else {
            throw new Error(`hole contains ${exp.kind}`);
        }
    }

    traceLabel(name: string): void {
        let exp = this.getCurrentExp();
        if (exp.kind === 'unknown') {
            let labelBody = [unknown()];
            this.setExp(label(name, labelBody));
            this.enterBlock({ body: labelBody, index: 0 });
        }
        else if (exp.kind === 'label') {
            if (exp.name !== name) {
                throw new Error(`Cannot merge label with name ${name} into label with
                    name ${exp.name}`);
            }
            this.mayIncrementCursor();
            this.enterBlock({ body: exp.body, index: 0 });
        }
        else {
            throw new Error(`expected label, got ${exp.kind}`);
        }
    }

    traceBreak(name: string, value: Exp): void {
        let exp = this.getCurrentExp();
        if (exp.kind === 'unknown') {
            this.setExp(break_(name, value));
        }
        else if (exp.kind === 'break') {
            if (exp.name !== name) {
                throw new Error(`Cannot merge break with name ${name} into break with
                    name ${exp.name}`);
            }
            exp.value = mergeExp(exp.value, value);
            this.mayIncrementCursor();
        }
        else {
            throw new Error(`expected break, got ${exp.kind}`);
        }

        // Rewind
        // Keep exiting until you reach the correct label.
        this.exitBlock();
        let prev = this.getPrevExp();
        while((prev.kind === 'label' && prev.name !== name) || prev.kind !== 'label') {
            this.quietExitBlock();
            prev = this.getPrevExp();
        }

        // Resume normal control flow.
        if (prev.kind !== 'label') {
            throw new Error("Expected label!");
        }
        // prev.kind === 'label' && prev.name === name

    }

    traceReturn(e1: Exp): void {
        let e2 = this.getCurrentExp();
        if (e2.kind === 'unknown') {
            // This may need to be some { kind: 'return', body: exp };
            this.setExp(e1);
        }
        else {
            mergeExp(e2, e1);
        }
    }


    prettyPrint(): void {
        console.log(JSON.stringify(this.getTrace() , null, 2));
    }

}

export function newTrace() {
    return new Trace([unknown()]);
}

function mergeExpArray(e1: Exp[], e2: Exp[]): Exp[] {
    // TODO(arjun): This may be wrong. What if one is [ unknown ] and the
    // other is [ 1 , 2 ]. Shouldn't the merge be [ 1, 2 ]?
    if (e1.length !== e2.length) {
        throw new Error('uneven lengths in block');
    }
    for (let i = 0; i < e1.length; i = i + 1) {
        e1[i] = mergeExp(e1[i], e2[i]);
    }
    return e1;

}

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
        return e1;
    }
    else if (e1.kind === 'set' && e2.kind === 'set') {
        if (e1.name !== e2.name) {
            throw new Error(`Cannot merge set expressions naming ${e1.name}
                and ${e2.name}`);
        }
        e1.named = mergeExp(e1.named, e2.named);
        return e1;
    }
    else if (e1.kind === 'if' && e2.kind === 'if') {
        e1.cond = mergeExp(e1.cond, e2.cond);
        e1.truePart = mergeExpArray(e1.truePart, e2.truePart);
        e1.falsePart = mergeExpArray(e1.falsePart, e2.falsePart);
        return e1;
    }
    else if (e1.kind === 'binop' && e2.kind === 'binop') {
        if (e1.op !== e2.op) {
            throw new Error(`Cannot merge operations ${e1.op} and ${e2.op}`);
        }
        e1.e1 = mergeExp(e1.e1, e2.e1);
        e1.e2 = mergeExp(e1.e2, e2.e2);
        return e1;
    }
    else if (e1.kind === 'block' && e2.kind === 'block') {
        e1.body = mergeExpArray(e1.body, e2.body);
        return e1;
    }
    else if (e1.kind === 'label' && e2.kind === 'label') {
        if(e1.name !== e2.name) {
            throw new Error(`Mismatched names ${e1.name} and ${e2.name}.`);
        }
        e1.body = mergeExpArray(e1.body, e2.body);
        return e1;
    }
    else if (e1.kind === 'break' && e2.kind === 'break') {
        if(e1.name !== e2.name) {
            throw new Error(`Mismatched names ${e1.name} and ${e2.name}.`);
        }
        return e1;
    }
    else {
        // Note: We do not support merging callbacks here. traceCallback takes
        // care of that.
        throw new Error(`Cannot merge expressions of kinds ${e1.kind} and
            ${e2.kind}`);
    }
}