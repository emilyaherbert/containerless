"use strict";
exports.__esModule = true;
function expectIfStmt(e) {
    if (e.kind !== 'if') {
        throw new Error("Expected if.");
    }
    else {
        return e;
    }
}
exports.expectIfStmt = expectIfStmt;
function expectWhileStmt(e) {
    if (e.kind !== 'while') {
        throw new Error("Expected while.");
    }
    else {
        return e;
    }
}
exports.expectWhileStmt = expectWhileStmt;
function expectReturnStmt(e) {
    if (e.kind !== 'return') {
        throw new Error("Expected return.");
    }
    else {
        return e;
    }
}
exports.expectReturnStmt = expectReturnStmt;
function expectIdExp(e) {
    if (e.kind !== 'identifier') {
        throw new Error("Expected id.");
    }
    else {
        return e;
    }
}
exports.expectIdExp = expectIdExp;
function expectObject(e) {
    if (e.kind !== 'object') {
        throw new Error("Expected object.");
    }
    else {
        return e;
    }
}
exports.expectObject = expectObject;
function expectMemberExp(e) {
    if (e.kind !== 'member') {
        throw new Error("Expected object.");
    }
    else {
        return e;
    }
}
exports.expectMemberExp = expectMemberExp;
function expectObjectTyp(t) {
    if (t.kind !== 'object') {
        throw new Error("Expected object type.");
    }
    else {
        return t;
    }
}
exports.expectObjectTyp = expectObjectTyp;
function unwrapObject(e) {
    if (e.kind !== 'object') {
        throw new Error("Expected object.");
    }
    else {
        return e.value;
    }
}
exports.unwrapObject = unwrapObject;
function unwrapBoolean(e) {
    switch (e.kind) {
        case 'boolean': return e.value;
        default: throw new Error("Expected boolean in unwrap_boolean.");
    }
}
exports.unwrapBoolean = unwrapBoolean;
