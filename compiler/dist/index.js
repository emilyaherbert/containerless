"use strict";
exports.__esModule = true;
var babel = require("@babel/core");
var fs = require("fs");
var t = require("@babel/types");
var insertTracing = require("./insertTracing");
var code = fs.readFileSync('input.js', { encoding: 'utf-8' });
var ast = babel.parseSync(code);
if (ast === null) {
    throw 'something wrong';
}
if (!t.isFile(ast)) {
    throw 'something wrong';
}
var result = babel.transformSync(code, {
    plugins: [insertTracing.plugin],
    ast: true
});
// console.log(result!.ast);
console.log(result.code);
