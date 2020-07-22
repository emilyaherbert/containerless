#!/usr/bin/env node
import * as fs from 'fs';
import * as r from './insertTracing';

function inputFile(): string | number {
    if (process.argv.length === 2) {
        // 0 is standard input. According to StackOverflow, this is the
        // Right Way to do this https://stackoverflow.com/questions/30441025/.
        return 0;
    }
    else if (process.argv.length === 3) {
        return process.argv[2];
    }
    else {
        console.error('Expected filename on command line or standard input');
        return process.exit(2);
    }
}

let inputCode = fs.readFileSync(inputFile(), { encoding: 'utf-8' });
console.log(r.transform(inputCode));