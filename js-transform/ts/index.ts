import * as fs from 'fs';
import * as r from './insertTracing';

let inputCode = fs.readFileSync(process.argv[2], { encoding: 'utf-8' });
r.transform(inputCode);