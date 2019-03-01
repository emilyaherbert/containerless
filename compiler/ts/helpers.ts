import { Stmt, If, While, Return, Exp, Id, Obj } from "../ts/types";

export function expect_if(e: Stmt): If {
  if(e.kind !== 'if') {
    throw new Error("Expected if.");
  } else {
    return e;
  }
}

export function expect_while(e: Stmt): While {
  if(e.kind !== 'while') {
    throw new Error("Expected while.");
  } else {
    return e;
  }
}

export function expect_return(e: Stmt): Return {
  if(e.kind !== 'return') {
    throw new Error("Expected return.");
  } else {
    return e;
  }
}

export function expect_identifier(e: Exp): Id {
  if(e.kind !== 'identifier') {
    throw new Error("Expected id.");
  } else {
    return e;
  }
}

export function expect_object(e: Exp): Object {
  if(e.kind !== 'object') {
    throw new Error("Expected object.");
  } else {
    return e;
  }
}

export function unwrap_object(e: Exp): Obj {
  if(e.kind !== 'object') {
    throw new Error("Expected object.");
  } else {
    return e.value;
  }
}

export function unwrap_boolean(e: Exp): boolean {
  switch(e.kind) {
    case 'boolean': return e.value;
    default: throw new Error("Expected boolean in unwrap_boolean.");
  }
}