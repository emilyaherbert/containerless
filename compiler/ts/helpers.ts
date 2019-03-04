import { Stmt, IfStmt, WhileStmt, ReturnStmt, Exp, IdExp, ObjectTyp, Typ } from "../ts/types";

export function expectIfStmt(e: Stmt): IfStmt {
  if(e.kind !== 'if') {
    throw new Error("Expected if.");
  } else {
    return e;
  }
}

export function expectWhileStmt(e: Stmt): WhileStmt {
  if(e.kind !== 'while') {
    throw new Error("Expected while.");
  } else {
    return e;
  }
}

export function expectReturnStmt(e: Stmt): ReturnStmt {
  if(e.kind !== 'return') {
    throw new Error("Expected return.");
  } else {
    return e;
  }
}

export function expectIdExp(e: Exp): IdExp {
  if(e.kind !== 'identifier') {
    throw new Error("Expected id.");
  } else {
    return e;
  }
}

export function expectObject(e: Exp): Object {
  if(e.kind !== 'object') {
    throw new Error("Expected object.");
  } else {
    return e;
  }
}

export function expectObjectTyp(t: Typ): ObjectTyp {
  if(t.kind !== 'object') {
    throw new Error("Expected object type.");
  } else {
    return t;
  }
}

export function unwrapObject(e: Exp): { [key: string]: Exp } {
  if(e.kind !== 'object') {
    throw new Error("Expected object.");
  } else {
    return e.value;
  }
}

export function unwrapBoolean(e: Exp): boolean {
  switch(e.kind) {
    case 'boolean': return e.value;
    default: throw new Error("Expected boolean in unwrap_boolean.");
  }
}