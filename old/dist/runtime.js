"use strict";
exports.__esModule = true;
var helpers = require("./helpers");
var AST = /** @class */ (function () {
    function AST() {
        this.writing = false;
        this.program = [{ kind: 'unknown' }];
        this.stack = [this.program];
        this.currentRover = 0;
        // Class name to class implementation.
        this.classes = new Map();
        // Hashcode to class name.
        this.hashCodeMap = new Map();
        // Variable name to class name.
        // Write only.
        this.tenv = new Map();
    }
    AST.prototype.current = function () {
        if (this.stack.length > 0) {
            return this.stack[this.stack.length - 1];
        }
        else {
            throw new Error("Expected nonempty this.stack.");
        }
    };
    AST.prototype.push = function (e) {
        var current = this.current();
        if (this.hasNext() && this.next().kind === 'unknown') {
            this.writing = true;
            current.pop();
        }
        if (this.writing) {
            current.push(e);
        }
        this.currentRover++;
    };
    AST.prototype.pop = function () {
        if (this.writing) {
            this.current().pop();
        }
        this.currentRover--;
    };
    AST.prototype.hasPrev = function () {
        var current = this.current();
        return (current.length > 0 &&
            this.currentRover > 0 &&
            current.length > (this.currentRover - 1));
    };
    AST.prototype.prev = function () {
        if (this.hasPrev()) {
            return this.current()[this.currentRover - 1];
        }
        else {
            throw new Error("No previous Stmt.");
        }
    };
    AST.prototype.hasNext = function () {
        var current = this.current();
        return (current.length > 0 &&
            this.currentRover > -1 &&
            current.length > this.currentRover);
    };
    AST.prototype.next = function () {
        if (this.hasNext()) {
            return this.current()[this.currentRover];
        }
        else {
            throw new Error("No next Stmt.");
        }
    };
    AST.prototype.pushScope = function () {
        this.stack.push([]);
        this.currentRover = 0;
    };
    AST.prototype.pushScopeWith = function (stmts) {
        this.stack.push(stmts);
        this.currentRover = 0;
    };
    AST.prototype.popScope = function () {
        this.stack.pop();
        this.currentRover = this.current().length;
    };
    AST.prototype.getProgram = function () {
        return this.program;
    };
    AST.prototype.setClass = function (hashCode, c) {
        this.classes.set(c.name, c);
        this.hashCodeMap.set(hashCode, c.name);
    };
    AST.prototype.getClass = function (name) {
        if (!this.classes.has(name)) {
            throw new Error("No matching class found.");
        }
        else {
            return this.classes.get(name);
        }
    };
    AST.prototype.findClass = function (types) {
        var hashCode = createTypeMapHashCode(types);
        if (this.hashCodeMap.has(hashCode)) {
            var name = this.hashCodeMap.get(hashCode);
            if (this.classes.has(name)) {
                return this.classes.get(name);
            }
            else {
                return undefined;
            }
        }
        else {
            return undefined;
        }
    };
    AST.prototype.setTEnv = function (v, t) {
        if (this.tenv.has(v)) {
            this.tenv.get(v).push(t);
        }
        else {
            this.tenv.set(v, [t]);
        }
    };
    AST.prototype.getClasses = function () {
        return Array.from(this.classes.values());
    };
    AST.prototype.getTEnv = function () {
        return this.tenv;
    };
    AST.prototype.refresh = function () {
        this.stack = [this.program];
        this.currentRover = 0;
        this.writing = false;
    };
    return AST;
}());
exports.AST = AST;
var ast = new AST();
var args_stack = [];
function startTrace() {
    ast.refresh();
}
exports.startTrace = startTrace;
var nextName = 0;
function bind(e) {
    var name = nextName++;
    var t = getTyp(e);
    ast.setTEnv(name, t);
    ast.push({ kind: 'let', name: name, body: e });
    return { kind: 'identifier', name: name, type: t };
}
exports.bind = bind;
function updateByName(e1, e2) {
    var idExp = helpers.expectIdExp(e1);
    var t = getTyp(e2);
    ast.setTEnv(idExp.name, t);
    ast.push({ kind: 'assignment', e1: e1, e2: e2 });
}
exports.updateByName = updateByName;
// Should only be used to update contents of objects, arrays, etc.
function updateByExp(e1, e2) {
    ast.push({ kind: 'assignment', e1: e1, e2: e2 });
}
exports.updateByExp = updateByExp;
function input() {
    return { kind: 'input' };
}
exports.input = input;
function args(es) {
    args_stack.push(es);
}
exports.args = args;
function params() {
    if (args_stack.length > 0) {
        var es = args_stack[args_stack.length - 1];
        args_stack.pop();
        return es.map(bind);
    }
    else {
        throw new Error("Found empty args_stack.");
    }
}
exports.params = params;
function return_(value) {
    ast.push({ kind: 'return', value: value });
}
exports.return_ = return_;
function expectReturn() {
    var theReturn = helpers.expectReturnStmt(ast.prev());
    // TODO(emily): In order to do multiple inputs, the AST cannot lose track of returns.
    // What we were doing before was removing the returns when we found an 'expectReturn',
    // but if we do that for multiple traces, the 'expectReturns' throw errors as they
    // *expect returns*, and there are none.
    // Not an issue now, but possibly an issue when compiling to rust as we cannot
    // blindly remove them because of how main works. -> Change main?
    //ast.pop();
    return bind(theReturn.value);
}
exports.expectReturn = expectReturn;
function if_(test) {
    ast.push({ kind: 'if',
        test: test,
        then_part: { kind: 'block', body: [{ kind: 'unknown' }] },
        else_part: { kind: 'block', body: [] } });
}
exports.if_ = if_;
function ifElse(test) {
    ast.push({
        kind: 'if',
        test: test,
        then_part: { kind: 'block', body: [{ kind: 'unknown' }] },
        else_part: { kind: 'block', body: [{ kind: 'unknown' }] }
    });
}
exports.ifElse = ifElse;
function enterIf(condition) {
    var theIf = helpers.expectIfStmt(ast.prev());
    // TODO(arjun): Test condition
    if (condition) {
        if (theIf.then_part.kind === 'block') {
            ast.pushScopeWith(theIf.then_part.body);
            theIf.then_part = { kind: 'block', body: ast.current() };
        }
    }
    else {
        if (theIf.else_part.kind === 'block') {
            ast.pushScopeWith(theIf.else_part.body);
            theIf.else_part = { kind: 'block', body: ast.current() };
        }
    }
}
exports.enterIf = enterIf;
function exitIf() {
    ast.popScope();
}
exports.exitIf = exitIf;
function while_(test) {
    ast.push({
        kind: 'while',
        test: test,
        body: { kind: 'unknown' }
    });
}
exports.while_ = while_;
function enterWhile() {
    var theWhile = helpers.expectWhileStmt(ast.prev());
    ast.pushScope();
    theWhile.body = { kind: 'block', body: ast.current() };
}
exports.enterWhile = enterWhile;
function exitWhile() {
    ast.popScope();
}
exports.exitWhile = exitWhile;
var unaryOpReturnType = new Map();
var binOpReturnType = new Map();
function genericUnaryOp(opStr, inputType, returnType) {
    unaryOpReturnType.set(opStr, returnType);
    return function (e) {
        if (getTyp(e).kind === inputType.kind) {
            return { kind: 'unaryop', op: opStr, e: e };
        }
        else {
            throw new Error('Not implemented.');
        }
    };
}
function genericBinOp(opStr, inputType, returnType) {
    binOpReturnType.set(opStr, returnType);
    return function (e1, e2) {
        if (getTyp(e1).kind === inputType.kind &&
            getTyp(e2).kind === inputType.kind) {
            return { kind: 'binop', op: opStr, e1: e1, e2: e2 };
        }
        else {
            throw new Error('Not implemented.');
        }
    };
}
// TODO(Chris): Many of these operations were very haphazardly named, and the export names
// may need to change if we have separate exported functions like +num and +str, etc.
exports.neg = genericUnaryOp('unary-', { kind: 'number' }, { kind: 'number' });
exports.plus = genericUnaryOp('unary+', { kind: 'number' }, { kind: 'number' });
exports.not = genericUnaryOp('!', { kind: 'boolean' }, { kind: 'boolean' });
exports.bitnot = genericUnaryOp('~', { kind: 'number' }, { kind: 'number' });
// TODO(Chris): should we just cut to the chase and return "undefined" here?
function _void(e) {
    return { kind: 'unaryop', op: 'void', e: e };
}
exports._void = _void;
exports.eq = genericBinOp('==num', { kind: 'number' }, { kind: 'boolean' });
exports.ineq = genericBinOp('!=num', { kind: 'number' }, { kind: 'boolean' });
exports.exacteq = genericBinOp('===num', { kind: 'number' }, { kind: 'boolean' });
exports.exactineq = genericBinOp('!==num', { kind: 'number' }, { kind: 'boolean' });
exports.lt = genericBinOp('<', { kind: 'number' }, { kind: 'boolean' });
exports.gt = genericBinOp('>', { kind: 'number' }, { kind: 'boolean' });
exports.leq = genericBinOp('<=', { kind: 'number' }, { kind: 'boolean' });
exports.geq = genericBinOp('>=', { kind: 'number' }, { kind: 'boolean' });
exports.sub = genericBinOp('-', { kind: 'number' }, { kind: 'number' });
exports.div = genericBinOp('/', { kind: 'number' }, { kind: 'number' });
exports.mul = genericBinOp('*', { kind: 'number' }, { kind: 'number' });
exports.remainder = genericBinOp('%', { kind: 'number' }, { kind: 'number' });
exports.pow = genericBinOp('**', { kind: 'number' }, { kind: 'number' });
exports.lshift = genericBinOp('<<', { kind: 'number' }, { kind: 'number' });
exports.rshift = genericBinOp('>>', { kind: 'number' }, { kind: 'number' });
exports.unsignedrshift = genericBinOp('>>>', { kind: 'number' }, { kind: 'number' });
exports.bitand = genericBinOp('&', { kind: 'number' }, { kind: 'number' });
exports.bitor = genericBinOp('|', { kind: 'number' }, { kind: 'number' });
exports.bitxor = genericBinOp('^', { kind: 'number' }, { kind: 'number' });
exports.and = genericBinOp('&&', { kind: 'boolean' }, { kind: 'boolean' });
exports.or = genericBinOp('||', { kind: 'boolean' }, { kind: 'boolean' });
binOpReturnType.set('+num', { kind: 'number' });
binOpReturnType.set('+str', { kind: 'string' });
function add(e1, e2) {
    if (getTyp(e1).kind === 'number' &&
        getTyp(e2).kind === 'number') {
        return { kind: 'binop', op: '+num', e1: e1, e2: e2 };
    }
    else if (getTyp(e1).kind === 'string' &&
        getTyp(e2).kind === 'string') {
        return { kind: 'binop', op: '+str', e1: e1, e2: e2 };
    }
    else {
        throw new Error('Not implemented.');
    }
}
exports.add = add;
function ternary(test, consequent, alternate) {
    if (getTyp(test).kind === 'boolean') {
        return { kind: 'ternary', test: test, consequent: consequent, alternate: alternate };
    }
    else {
        throw new Error("Ternary expression test is " + getTyp(test).kind + ", not a boolean.");
    }
}
exports.ternary = ternary;
function num(n) {
    return { kind: 'number', value: n };
}
exports.num = num;
function str(s) {
    return { kind: 'string', value: s };
}
exports.str = str;
function bool(b) {
    return { kind: 'boolean', value: b };
}
exports.bool = bool;
function undefined_() {
    return { kind: 'undefined' };
}
exports.undefined_ = undefined_;
function object(o) {
    var myClass = resolveClass(o);
    return { kind: 'object', "class": myClass.name, value: o };
}
exports.object = object;
function resolveClass(o) {
    var types = new Map();
    for (var key in o) {
        types.set(key, getTyp(o[key]));
    }
    var existingClass = ast.findClass(types);
    if (existingClass === undefined) {
        return createNewClass(types);
    }
    else {
        return existingClass;
    }
}
var nextClassName = 0;
function createNewClass(types) {
    var name = nextClassName++;
    var hashCode = createTypeMapHashCode(types);
    var transitions = new Map();
    var newClass = { kind: 'class', name: name, types: types, transitions: transitions };
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
function createTypeMapHashCode(types) {
    function createHashCode(s) {
        for (var i = 0, h = 0; i < s.length; i++)
            h = Math.imul(31, h) + s.charCodeAt(i) | 0;
        return h;
    }
    var hashCode = 0;
    types.forEach(function (value, key) {
        hashCode += createHashCode(key + value.kind);
    });
    return hashCode;
}
function member(o, f) {
    var id = helpers.expectIdExp(o);
    return { kind: 'member', object: id, field: f };
}
exports.member = member;
function updateObject(o, e) {
    // NOTE(emily): Exp is confusing here because o is only on the LHS of assignment.
    // But it has to be this way because $T statements arguments are javascript id's.
    var memberExp = helpers.expectMemberExp(o);
    var idExp = memberExp.object;
    var objectTyp = helpers.expectObjectTyp(getTyp(idExp));
    var myClass = ast.getClass(objectTyp["class"]);
    if (myClass.types.hasOwnProperty(memberExp.field)) {
        updateByExp(memberExp, e);
    }
    else {
        var newTypes = new Map(myClass.types);
        var eType = getTyp(e);
        newTypes.set(memberExp.field, eType);
        var newClass = createNewClass(newTypes);
        myClass.transitions.set([memberExp.field, eType], newClass.name);
        // Typ of object in member expression.
        var newTyp = ({
            kind: 'object',
            "class": newClass.name
        });
        // Rebuilt id handle on object in member expression.
        var newIdExp = ({
            kind: 'identifier',
            name: memberExp.object.name,
            type: newTyp
        });
        // Update the typ env with updated typ for object.
        ast.setTEnv(newIdExp.name, newTyp);
        var newMemberExp = ({
            kind: 'member',
            object: newIdExp,
            field: memberExp.field
        });
        // Update the member itself.
        updateByExp(newMemberExp, e);
    }
}
exports.updateObject = updateObject;
function log() {
    console.log(JSON.stringify({ kind: 'block', body: ast.getProgram() }, null, 2));
}
exports.log = log;
// TODO(emily): Used for testing, replace with something more elegant.
// i.e. https://github.com/plasma-umass/ElementaryJS/blob/master/ts/runtime.ts#L316
function clear() {
    ast = new AST();
    args_stack = [];
    nextName = 0;
    nextClassName = 0;
}
exports.clear = clear;
function getProgram() {
    return ast.getProgram();
}
exports.getProgram = getProgram;
function getProgramAsString() {
    return JSON.stringify({ kind: 'block', body: ast.getProgram() }, null, 2);
}
exports.getProgramAsString = getProgramAsString;
function getClasses() {
    return ast.getClasses();
}
exports.getClasses = getClasses;
function getTEnv() {
    return ast.getTEnv();
}
exports.getTEnv = getTEnv;
function getTyp(e) {
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
            return unaryOpReturnType.get(e.op);
        }
        else {
            throw new Error("Found unimplemented unaryop.");
        }
    }
    else if (e.kind === 'binop') {
        if (binOpReturnType.has(e.op)) {
            return binOpReturnType.get(e.op);
        }
        else {
            throw new Error("Found unimplemented binop.");
        }
    }
    else if (e.kind === 'ternary') {
        var consequentType = getTyp(e.consequent);
        var alternateType = getTyp(e.alternate);
        if (consequentType.kind !== alternateType.kind) {
            // TODO(Chris): for now
            throw new Error("Consequent and alternate have different types, " + consequentType.kind + " and " + alternateType.kind + ".");
        }
        else {
            return consequentType;
        }
    }
    else if (e.kind === 'input') {
        return { kind: 'number' }; // TODO(arjun): For now
    }
    else if (e.kind === 'object') {
        return { kind: 'object', "class": e["class"] };
    }
    else if (e.kind === 'member') {
        var objectTyp = helpers.expectObjectTyp(getTyp(e.object));
        var myClass = ast.getClass(objectTyp["class"]);
        if (myClass.types.has(e.field)) {
            return myClass.types.get(e.field);
        }
        else {
            throw new Error("Field not found in class type map.");
        }
    }
    else {
        console.log(e);
        throw new Error('Not implemented.');
    }
}
