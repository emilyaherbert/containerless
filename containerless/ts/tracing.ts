// import { Exp, eLet } from './types';



// // type Trace = (hole: Exp) => Exp | Exp;
// let partialTrace: (body: Exp) => Exp = (body: Exp) => {
//     return body;
// }

// let nextName = 0;

// export function traceLet(named: Exp) {
//     let name = `x${nextName}`;
//     nextName = nextName + 1;
//     partialTrace = (body) => {
//         return eLet(name, named, body);
//     };
// }

