import { Stmt, Exp, If, While } from "../ts/types";

export function expect_if(e: Stmt): If {
  if(e.kind !== 'if') {
    throw new Error("Expected if.");
  } else {
    return e;
  }
}

export function expect_while(e: Stmt): While {
  if(e.kind !== 'while') {
    throw new Error("Expected if.");
  } else {
    return e;
  }
}