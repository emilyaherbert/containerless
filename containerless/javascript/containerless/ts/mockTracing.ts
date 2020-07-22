import { TracingInterface } from './types';
import {
    Exp,  unknown, LVal
} from './exp';


class MockTrace implements TracingInterface {

    constructor() {
    }

    pushArgs(exps: Exp[]) {

    }

    popArgs(): Exp[] {
        let u = unknown();
        return [u, u, u, u, u, u]; // should be enough!
    }

    getTrace(): Exp {
        return unknown();
    }

    newTrace() {
    }

    exitBlock() {
    }

    /**
     * Creates the trace expression 'let name = { unknown };' and enters the
     * block containing the 'unknown'.
     */
    traceNamed(name: string): void {
    }

    traceLet(name: string, named: Exp): void {
    }

    traceFunctionCall(name: string, theArgs: Exp[]): void {
    }

    traceFunctionBody(labelName: string): Exp[] {
        return [];
    }

    traceSet(name: LVal, named: Exp): void {

    }

    traceIfTrue(cond: Exp) {
    }

    traceIfFalse(cond: Exp) {
    }

    traceWhile(cond: Exp) {
    }

    traceLoop() {
    }

    traceCallback(event: string, eventArg: Exp, callbackArgs: string[], clos: Exp): MockTrace {
        return new MockTrace();
    }

    tracePrimApp(event: string, eventArgs: Exp[]) {
    }

    traceLabel(name: string): void {
    }

    traceBreak(name: string, value: Exp): void {
    }

    traceReturn(e1: Exp): void {
    }


    prettyPrint(): void {
    }

}

export function newMockTrace(): TracingInterface {
    return new MockTrace();
}