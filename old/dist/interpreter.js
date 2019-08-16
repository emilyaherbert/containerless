"use strict";
exports.__esModule = true;
var helpers = require("./helpers");
var State = /** @class */ (function () {
    function State() {
        this.names = new Map([]);
        this.stack = [];
        this.values = [];
    }
    // Save the names that were declared in the current scope.
    // Start new scope.
    State.prototype.push = function () {
        this.stack.push(new Map(this.names));
        this.names = new Map([]);
    };
    // Set current scope to previous scope.
    // Throw out top scope.
    State.prototype.pop = function () {
        if (this.stack.length > 0) {
            this.names = this.stack[this.stack.length - 1];
        }
        this.stack.pop();
    };
    // Check if declared in current scope.
    // Traverse through previous scopes from closest to furthest.
    State.prototype.has = function (id) {
        if (this.names.has(id)) {
            return true;
        }
        for (var i = this.stack.length - 1; i > (-1); i--) {
            if (this.stack[i].has(id)) {
                return true;
            }
        }
        return false;
    };
    // Add to current scope.
    State.prototype.set = function (id, e) {
        this.names.set(id, e);
    };
    // Check if declared in current scope.
    // Traverse through previous scopes from closest to furthest.
    State.prototype.get = function (id) {
        if (this.names.has(id)) {
            return this.names.get(id);
        }
        for (var i = this.stack.length - 1; i > (-1); i--) {
            if (this.stack[i].has(id)) {
                return this.stack[i].get(id);
            }
        }
    };
    // Update most recent let from closest scope.
    State.prototype.update = function (id, e) {
        if (this.names.has(id)) {
            this.names.set(id, e);
            return;
        }
        for (var i = this.stack.length - 1; i > (-1); i--) {
            if (this.stack[i].has(id)) {
                this.stack[i].set(id, e);
                return;
            }
        }
    };
    return State;
}());
var Interpreter = /** @class */ (function () {
    function Interpreter() {
        this.st = new State();
    }
    // Entry-point.
    Interpreter.prototype.eval = function (e, input) {
        this.st = new State();
        for (var i = 0; i < e.length; i++) {
            this.eval_stmt(e[i], input);
        }
        if (this.st.values.length > 0)
            return this.st.values[this.st.values.length - 1];
        else
            throw new Error("No values!");
    };
    Interpreter.prototype.eval_stmt = function (e, input) {
        switch (e.kind) {
            case 'let': {
                var v = this.eval_exp(e.body, input);
                this.st.set(e.name, v);
                break;
            }
            case 'assignment': {
                // TODO(Chris): this should be refactored
                if (e.e1.kind === 'identifier') {
                    this.st.update(e.e1.name, this.eval_exp(e.e2, input));
                }
                else if (e.e1.kind === 'member') {
                    // TODO(Chris): not sure if some kind of st.update is needed?
                    // modifying the object should affect it for all references, since
                    // we are not creating a copy
                    var o = this.eval_exp(e.e1.object, input);
                    helpers.unwrapObject(o)[e.e1.field] = this.eval_exp(e.e2, input);
                }
                else {
                    throw new Error('Invalid assignment expression.');
                }
                break;
            }
            case 'if': {
                var c = this.eval_exp(e.test, input);
                if (helpers.unwrapBoolean(c))
                    this.eval_stmt(e.then_part, input);
                else
                    this.eval_stmt(e.else_part, input);
                break;
            }
            case 'while': {
                while (helpers.unwrapBoolean(this.eval_exp(e.test, input))) {
                    this.eval_stmt(e.body, input);
                }
                break;
            }
            case 'block': {
                this.st.push();
                for (var i = 0; i < e.body.length; i++) {
                    this.eval_stmt(e.body[i], input);
                }
                this.st.pop();
                break;
            }
            case 'return': {
                var v = this.eval_exp(e.value, input);
                this.st.values.push(v);
                break;
            }
            case 'unknown': throw new Error("Found unimplemented unknown case in eval_stmt.");
            default: throw new Error("Found unimplemented kind " + e.kind + " in eval_stmt.");
        }
    };
    Interpreter.prototype.eval_exp = function (e, input) {
        switch (e.kind) {
            case 'number': return e;
            case 'string': return e;
            case 'boolean': return e;
            case 'undefined': return e;
            case 'input': return input;
            case 'unaryop': {
                var v = this.eval_exp(e.e, input);
                return this.eval_unaryop(e.op, v);
            }
            case 'binop': {
                var v1 = this.eval_exp(e.e1, input);
                var v2 = this.eval_exp(e.e2, input);
                return this.eval_binop(e.op, v1, v2);
            }
            case 'ternary': {
                var test_1 = this.eval_exp(e.test, input);
                if (test_1.kind !== 'boolean') {
                    throw new Error("ternary test did not evaluate to a boolean.");
                }
                if (helpers.unwrapBoolean(test_1)) {
                    return this.eval_exp(e.consequent, input);
                }
                else {
                    return this.eval_exp(e.alternate, input);
                }
            }
            case 'identifier': {
                if (this.st.has(e.name)) {
                    var v = this.st.get(e.name);
                    if (typeof (v) !== 'undefined') {
                        return v;
                    }
                    else {
                        throw new Error("Found undefined identifier.");
                    }
                }
                else {
                    throw new Error("Found free identifier.");
                }
            }
            case 'object': {
                var obj = e.value;
                var innerObj = {};
                for (var key in obj) {
                    if (obj.hasOwnProperty(key)) {
                        innerObj[key] = this.eval_exp(obj[key], input);
                    }
                }
                return { kind: 'object', "class": e["class"], value: innerObj };
            }
            case 'member': {
                var o = this.eval_exp(e.object, input);
                return helpers.unwrapObject(o)[e.field];
            }
            default: throw new Error("Found unimplemented e.kind in eval_exp.");
        }
    };
    Interpreter.prototype.eval_unaryop = function (op, v) {
        if (op === 'void')
            return { kind: 'undefined' };
        if (v.kind === 'number') {
            switch (op) {
                case 'unary-': return { kind: 'number', value: -v.value };
                case 'unary+': return { kind: 'number', value: v.value };
                case '~': return { kind: 'number', value: ~v.value };
                default: throw new Error("Found unimplemented op in eval_unaryop.");
            }
        }
        else if (v.kind === 'boolean' && op === '!') {
            return { kind: 'boolean', value: !v.value };
        }
        else {
            throw new Error("Found unimplemented op in eval_unaryop.");
        }
    };
    // TODO(emily): Sync this with op's in runtime.ts.
    Interpreter.prototype.eval_binop = function (op, v1, v2) {
        if (v1.kind === 'number' && v2.kind === 'number') {
            switch (op) {
                case '+num': return { kind: 'number', value: (v1.value + v2.value) };
                case '-': return { kind: 'number', value: (v1.value - v2.value) };
                case '*': return { kind: 'number', value: (v1.value * v2.value) };
                case '/': return { kind: 'number', value: (v1.value / v2.value) };
                case '<': return { kind: 'boolean', value: (v1.value < v2.value) };
                case '<=': return { kind: 'boolean', value: (v1.value <= v2.value) };
                case '>': return { kind: 'boolean', value: (v1.value > v2.value) };
                case '>=': return { kind: 'boolean', value: (v1.value >= v2.value) };
                case '==': return { kind: 'boolean', value: (v1.value == v2.value) };
                case '===': return { kind: 'boolean', value: (v1.value === v2.value) };
                case '===num': return { kind: 'boolean', value: (v1.value === v2.value) };
                case '!=': return { kind: 'boolean', value: (v1.value != v2.value) };
                case '!==': return { kind: 'boolean', value: (v1.value !== v2.value) };
                default: throw new Error("Found unimplemented " + op + " in eval_binop.");
            }
        }
        else if (v1.kind === 'boolean' && v2.kind === 'boolean') {
            switch (op) {
                case '&&': return { kind: 'boolean', value: (v1.value && v2.value) };
                case '||': return { kind: 'boolean', value: (v1.value || v2.value) };
                case '==': return { kind: 'boolean', value: (v1.value == v2.value) };
                case '===': return { kind: 'boolean', value: (v1.value === v2.value) };
                case '!=': return { kind: 'boolean', value: (v1.value != v2.value) };
                case '!==': return { kind: 'boolean', value: (v1.value !== v2.value) };
                default: throw new Error("Found unimplemented " + op + " in eval_binop.");
            }
        }
        else if (v1.kind === 'string' && v2.kind === 'string') {
            switch (op) {
                case '+str': return { kind: 'string', value: (v1.value + v2.value) };
                case '==': return { kind: 'boolean', value: (v1.value == v2.value) };
                case '===': return { kind: 'boolean', value: (v1.value === v2.value) };
                case '!=': return { kind: 'boolean', value: (v1.value != v2.value) };
                case '!==': return { kind: 'boolean', value: (v1.value !== v2.value) };
                default: throw new Error("Found unimplemented " + op + " in eval_binop.");
            }
        }
        else {
            throw new Error("Found mismatched v1.kind and v2.kind in eval_binop.");
        }
    };
    return Interpreter;
}());
exports.Interpreter = Interpreter;
