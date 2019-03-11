import { Name, IdExp, Typ, Exp, Stmt, MemberExp } from "../ts/types";
import * as helpers from "./helpers"

type Class = { kind: 'class',
  name: number,
  types: Map<string, Typ>,
  transitions : Map<[string, Typ], number>};

export class AST {
  constructor() {}

  private writing = false;

  private program : Stmt[] = [ { kind: 'unknown' }];
  private stack : Stmt[][] = [ this.program ];
  
  private currentRover = 0;

  private classes : Map<number, Class> = new Map();
  private hashCodeMap : Map<number, number> = new Map();

  public current(): Stmt[] {
    if(this.stack.length > 0) {
      return this.stack[this.stack.length - 1];
    } else {
      throw new Error("Expected nonempty this.stack.");
    }
  }

  public push(e: Stmt) {
    let current = this.current();
    if(this.hasNext() && this.next().kind === 'unknown') {
      this.writing = true;
      current.pop();
    }
    if(this.writing) {
      current.push(e);
    }
    this.currentRover++;
  }

  public pop() {
    if(this.writing) {
      this.current().pop();
    }
    this.currentRover--;
  }

  public hasPrev(): boolean {
    let current = this.current();
    return (current.length > 0 &&
      this.currentRover > 0 &&
      current.length > (this.currentRover - 1));
  }

  public prev(): Stmt {
    if(this.hasPrev()) {
      return this.current()[this.currentRover - 1];
    } else {
      throw new Error("No previous Stmt.");
    }
  }

  public hasNext(): boolean {
    let current = this.current();
    return (current.length > 0 &&
      this.currentRover > -1 &&
      current.length > this.currentRover);
  }

  public next(): Stmt {
    if(this.hasNext()) {
      return this.current()[this.currentRover];
    } else {
      throw new Error("No next Stmt.");
    }
  }

  public pushScope() {
    this.stack.push([]);
    this.currentRover = 0;
  }

  public pushScopeWith(stmts: Stmt[]) {
    this.stack.push(stmts);
    this.currentRover = 0;
  }

  public popScope() {
    this.stack.pop();
    this.currentRover = this.current().length;
  }

  public getProgram(): Stmt[] {
    return this.program;
  }

  public setClass(hashCode: number, c: Class) {
    this.classes.set(c.name, c);
    this.hashCodeMap.set(hashCode, c.name);
  }

  public getClass(name: number): Class {
    if(!this.classes.has(name)) {
      throw new Error("No matching class found.");
    } else {
      return this.classes.get(name)!;
    }
  }

  public findClass(types: Map<string, Typ>): (Class | undefined) {
    const hashCode = createTypeMapHashCode(types);
    if(this.hashCodeMap.has(hashCode)) {
      const name = this.hashCodeMap.get(hashCode)!;
      if(this.classes.has(name)) {
        return this.classes.get(name);
      } else {
        return undefined;
      }
    } else {
      return undefined;
    }
  }

  public getClasses(): Class[] {
    return Array.from(this.classes.values());
  }

  public refresh() {
    this.stack = [ this.program ];
    this.currentRover = 0;
    this.writing = false;
  }
}

let ast = new AST();
let args_stack : Exp[][] = [];

export function startTrace() {
  ast.refresh();
}

let nextName : Name = 0;
export function bind(e: Exp): IdExp {
    let name = nextName++;
    let t = getTyp(e);
    ast.push({ kind: 'let', name: name, e: e });
    return { kind: 'identifier', name: name, type: t };
}

export function update(e1: Exp, e2: Exp): void {
    ast.push({ kind: 'assignment', e1: e1, e2: e2 });
}

export function input(): Exp {
    return { kind: 'input' };
}

export function args(es: Exp[]) {
  args_stack.push(es);
}

export function params(): IdExp[] {
  if(args_stack.length > 0) {
    const es = args_stack[args_stack.length - 1];
    args_stack.pop();
    return es.map(bind);
  } else {
    throw new Error ("Found empty args_stack.");
  }
}

export function return_(value: Exp) {
  ast.push({ kind: 'return', value: value })
}

export function expectReturn(): Exp {
  let theReturn = helpers.expectReturnStmt(ast.prev());
  // TODO(emily): In order to do multiple inputs, the AST cannot lose track of returns.
  // What we were doing before was removing the returns when we found an 'expectReturn',
  // but if we do that for multiple traces, the 'expectReturns' throw errors as they
  // *expect returns*, and there are none.
  // Not an issue now, but possibly an issue when compiling to rust as we cannot
  // blindly remove them because of how main works. -> Change main?
  
  //ast.pop();
  return bind(theReturn.value);
}

export function if_(test: Exp) {
  ast.push({ kind: 'if',
      test: test,
      then: { kind: 'block', body: [ { kind: 'unknown' } ]},
      else: { kind: 'block', body: []} });
}

export function ifElse(test: Exp) {
    ast.push({
        kind: 'if',
        test: test,
        then: { kind: 'block', body: [ { kind: 'unknown' } ]},
        else: { kind: 'block', body: [ { kind: 'unknown' } ]} });
}

export function enterIf(condition: boolean) {
  let theIf = helpers.expectIfStmt(ast.prev());

  // TODO(arjun): Test condition
  if (condition) {
    if(theIf.then.kind === 'block') {
      ast.pushScopeWith(theIf.then.body);
      theIf.then = { kind: 'block', body: ast.current() };
    }
  }
  else {
    if(theIf.else.kind === 'block') {
      ast.pushScopeWith(theIf.else.body);
      theIf.else = { kind: 'block', body: ast.current() };
    }
  }
}

export function exitIf() {
  ast.popScope();
}

export function while_(test: Exp) {
  ast.push({
    kind: 'while',
    test: test,
    body: { kind: 'unknown' } });
}

export function enterWhile() {
  let theWhile = helpers.expectWhileStmt(ast.prev());
  ast.pushScope();
  theWhile.body = { kind: 'block', body: ast.current() };
}

export function exitWhile() {
  ast.popScope();
}

const unaryOpReturnType = new Map<string, Typ>();
const binOpReturnType = new Map<string, Typ>();

function genericUnaryOp(opStr: string, inputType: Typ, returnType: Typ): (e: Exp) => Exp {
    unaryOpReturnType.set(opStr, returnType);
    return function(e: Exp): Exp {
        if (getTyp(e).kind === inputType.kind) {
            return { kind: 'unaryop', op: opStr, e };
        } else {
            throw new Error('Not implemented.');
        }
    }
}

function genericBinOp(opStr: string, inputType: Typ, returnType: Typ): (e1: Exp, e2: Exp) => Exp {
    binOpReturnType.set(opStr, returnType);
    return function(e1: Exp, e2: Exp): Exp {
        if (getTyp(e1).kind === inputType.kind &&
            getTyp(e2).kind === inputType.kind) {
            return { kind: 'binop', op: opStr, e1, e2 };
        } else {
            throw new Error('Not implemented.');
        }
    }
}

// TODO(Chris): Many of these operations were very haphazardly named, and the export names
// may need to change if we have separate exported functions like +num and +str, etc.
export const neg            = genericUnaryOp('unary-', { kind: 'number' } , { kind: 'number' });
export const plus           = genericUnaryOp('unary+', { kind: 'number' } , { kind: 'number' });
export const not            = genericUnaryOp('!',      { kind: 'boolean' }, { kind: 'boolean' });
export const bitnot         = genericUnaryOp('~',      { kind: 'number' } , { kind: 'number' });

// TODO(Chris): should we just cut to the chase and return "undefined" here?
export function _void(e: Exp): Exp {
  return { kind: 'unaryop', op: 'void', e };
}

export const eq             = genericBinOp('==num',  { kind: 'number' } , { kind: 'boolean' });
export const ineq           = genericBinOp('!=num',  { kind: 'number' } , { kind: 'boolean' });
export const exacteq        = genericBinOp('===num', { kind: 'number' } , { kind: 'boolean' });
export const exactineq      = genericBinOp('!==num', { kind: 'number' } , { kind: 'boolean' });
export const lt             = genericBinOp('<',      { kind: 'number' } , { kind: 'boolean' });
export const gt             = genericBinOp('>',      { kind: 'number' } , { kind: 'boolean' });
export const leq            = genericBinOp('<=',     { kind: 'number' } , { kind: 'boolean' });
export const geq            = genericBinOp('>=',     { kind: 'number' } , { kind: 'boolean' });
export const sub            = genericBinOp('-',      { kind: 'number' } , { kind: 'number' });
export const div            = genericBinOp('/',      { kind: 'number' } , { kind: 'number' });
export const mul            = genericBinOp('*',      { kind: 'number' } , { kind: 'number' });
export const remainder      = genericBinOp('%',      { kind: 'number' } , { kind: 'number' });
export const pow            = genericBinOp('**',     { kind: 'number' } , { kind: 'number' });
export const lshift         = genericBinOp('<<',     { kind: 'number' } , { kind: 'number' });
export const rshift         = genericBinOp('>>',     { kind: 'number' } , { kind: 'number' });
export const unsignedrshift = genericBinOp('>>>',    { kind: 'number' } , { kind: 'number' });
export const bitand         = genericBinOp('&',      { kind: 'number' } , { kind: 'number' });
export const bitor          = genericBinOp('|',      { kind: 'number' } , { kind: 'number' });
export const bitxor         = genericBinOp('^',      { kind: 'number' } , { kind: 'number' });
export const and            = genericBinOp('&&',     { kind: 'boolean' }, { kind: 'boolean' });
export const or             = genericBinOp('||',     { kind: 'boolean' }, { kind: 'boolean' });

binOpReturnType.set('+num', { kind: 'number' });
binOpReturnType.set('+str', { kind: 'string' });
export function add(e1: Exp, e2: Exp): Exp {
    if (getTyp(e1).kind === 'number' &&
        getTyp(e2).kind === 'number') {
        return { kind: 'binop', op: '+num', e1, e2 };
    } else if (getTyp(e1).kind === 'string' &&
               getTyp(e2).kind === 'string') {
        return { kind: 'binop', op: '+str', e1, e2 };
    } else {
        throw new Error('Not implemented.');
    }
}

export function ternary(test: Exp, consequent: Exp, alternate: Exp): Exp {
    if (getTyp(test).kind === 'boolean') {
        return { kind: 'ternary', test, consequent, alternate };
    } else {
        throw new Error(`Ternary expression test is ${getTyp(test).kind}, not a boolean.`);
    }
}

export function num(n: number): Exp {
    return { kind: 'number', value: n };
}

export function str(s: string): Exp {
    return { kind: 'string', value: s };
}

export function bool(b: boolean): Exp {
  return { kind: 'boolean', value: b };
}

export function undefined_(): Exp {
  return { kind: 'undefined' };
}

export function object(o: { [key: string]: Exp }): Exp {
  let myClass = resolveClass(o);
  return { kind: 'object', class: myClass.name, value: o };
}

function resolveClass(o: { [key: string]: Exp }): Class {
  let types : Map<string, Typ> = new Map();
  for(var key in o) {
    types.set(key, getTyp(o[key]));
  }
  const existingClass = ast.findClass(types);
  if(existingClass === undefined) {
    return createNewClass(types);
  } else {
    return existingClass;
  }
}

let nextClassName = 0;
function createNewClass(types: Map<string, Typ>): Class {
  let name = nextClassName++;
  let hashCode = createTypeMapHashCode(types);
  let transitions : Map<[string, Typ], number> = new Map();
  let newClass : Class = { kind: 'class', name: name, types: types, transitions: transitions };
  ast.setClass(hashCode, newClass);
  return newClass;
}

/*

TODO(emily): Do we need to add transitions to the hash code?
I don't believe that we do but I'm not fully convinced...

We would need to add transitions to the hash code if we expected to encounter a
situation like:

Class A :
types : { x : number, y : number }
transitions : { z : C }

Class B :
types : { x : number, y : number }
transitions : { woah : D }

Class C :
...

Class D :
...

But I think because we are using hidden classes, we are gaurenteed that there can
exist no two classes that have the exact same type map, and if two objects have the
same type map, then they are intended to be the same hidden class.

*/
function createTypeMapHashCode(types : Map<string, Typ>): number {
  function createHashCode(s: string): number {
    for(var i = 0, h = 0; i < s.length; i++)
        h = Math.imul(31, h) + s.charCodeAt(i) | 0;
    return h;
  }

  let hashCode = 0;
  types.forEach((value, key) => {
    hashCode += createHashCode(key + value.kind);
  })

  return hashCode;
}

export function member(o: Exp, f: string): Exp {
  let id = helpers.expectIdExp(o);
  return { kind: 'member', object: id, field: f };
}

/*

TODO(emily): Currently, if an object needs to change it's hidden class,
it never does so 'officially'. The class is updated here, such that in the AST
the object will have Class A until it uses something new and becomes Class B.

I am *pretty sure* that this is the intended implementation of hidden classes,
but I am not sure how this will work with rust. We may have to enforce that all
hidden class updates are global through the whole AST. This would come with its
own issues though.

*/
export function updateObject(o: Exp, e: Exp) {
  let member = helpers.expectMemberExp(o);
  let objectTyp = helpers.expectObjectTyp(getTyp(member.object))
  let myClass = ast.getClass(objectTyp.class);
  if(myClass.types.hasOwnProperty(member.field)) {
    update(member, e);
  } else {
    let newTypes = new Map(myClass.types);
    let eType = getTyp(e);
    newTypes.set(member.field, eType);
    let newClass = createNewClass(newTypes);
    myClass.transitions.set([member.field, eType], newClass.name);

    let newMember : MemberExp = ({
      kind: 'member',
      object: ({
        kind: 'identifier',
        name: member.object.name,
        type: ({
          kind: 'object',
          class: newClass.name
        })
      }),
      field: member.field
    })

    update(newMember, e);
  }
}

export function log() {
    console.log(JSON.stringify(ast.getProgram(), null, 2));
}

// TODO(emily): Used for testing, replace with something more elegant.
// i.e. https://github.com/plasma-umass/ElementaryJS/blob/master/ts/runtime.ts#L316
export function clear() {
  ast = new AST();
  args_stack = [];
  nextName = 0;
  nextClassName = 0;
}

export function getProgram(): Stmt[] {
  return ast.getProgram();
}

export function getClasses(): Class[] {
  return ast.getClasses();
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
  else if (e.kind === 'unaryop') {
      if (unaryOpReturnType.has(e.op)) {
          return unaryOpReturnType.get(e.op)!;
      }
      else {
          throw new Error("Found unimplemented unaryop.");
      }
  }
  else if (e.kind === 'binop') {
      if (binOpReturnType.has(e.op)) {
          return binOpReturnType.get(e.op)!;
      }
      else {
          throw new Error("Found unimplemented binop.");
      }
  }
  else if (e.kind === 'ternary') {
      const consequentType = getTyp(e.consequent);
      const alternateType = getTyp(e.alternate);
      if (consequentType.kind !== alternateType.kind) {
          // TODO(Chris): for now
          throw new Error(`Consequent and alternate have different types, ${consequentType.kind} and ${alternateType.kind}.`);
      } else {
          return consequentType;
      }
  }
  else if (e.kind === 'input') {
      return { kind: 'number' }; // TODO(arjun): For now
  }
  else if (e.kind === 'object') {
    return { kind: 'object', class: e.class };
  }
  else if (e.kind === 'member') {
    const objectTyp = helpers.expectObjectTyp(getTyp(e.object));
    const myClass = ast.getClass(objectTyp.class);
    if(myClass.types.has(e.field)) {
      return myClass.types.get(e.field)!;
    } else {
      throw new Error("Field not found in class type map.");
    }
  }
  else {
    console.log(e);
    throw new Error('Not implemented.');
  }
}