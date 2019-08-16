"use strict";
exports.__esModule = true;
var babel = require("@babel/core");
var fs = require("fs");
var t = require("@babel/types");
var insertTracing = require("./insertTracing");
var $T = require('../dist/runtime');
var $R = require('../native');
var code = fs.readFileSync('input.js', { encoding: 'utf-8' });
var ast = babel.parseSync(code);
if (ast === null) {
    throw new Error("Found null ast.");
}
else if (!t.isFile(ast)) {
    throw new Error("Expected file ast.");
}
var trace = insertTracing.transform(code);
var func_output = eval(trace)(0);
var classes = $T.getClasses();
var program = $T.getProgram();
var programStr = $T.getProgramAsString();
$R.compile(programStr);
var result = $R.run();
console.log(result);
