import {
    Exp,  unknown, LVal
} from './exp';


export interface TracingInterface {
    pushArgs(exps: Exp[]): void;
    popArgs(): Exp[];
    getTrace(): Exp;
    newTrace(): void;
    exitBlock(): void;
    traceNamed(name: string): void;
    traceLet(name: string, named: Exp): void;
    traceFunctionCall(name: string, theArgs: Exp[]): void;
    traceFunctionBody(labelName: string): Exp[];
    traceSet(name: LVal, named: Exp): void;
    traceIfTrue(cond: Exp): void;
    traceIfFalse(cond: Exp): void;
    traceWhile(cond: Exp): void;
    traceLoop(): void;
    traceCallback(event: string, eventArg: Exp, callbackArgs: string[], clos: Exp): TracingInterface;
    tracePrimApp(event: string, eventArgs: Exp[]): void;
    traceLabel(name: string): void;
    traceBreak(name: string, value: Exp): void;
    traceReturn(e1: Exp): void;
    prettyPrint(): void;
}