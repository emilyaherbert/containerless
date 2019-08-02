#!/usr/bin/env node
import * as fs from 'fs';
import * as r from './insertTracing';

let args = process.argv;
if (args.length !== 3) {
    console.error('Expected one argument: filename.js');
    process.exit(1);
}

let code = fs.readFileSync(args[2], { encoding: 'utf-8' });
console.log(r.transform(code));