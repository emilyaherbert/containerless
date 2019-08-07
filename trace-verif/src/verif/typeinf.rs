use crate::verif::untyped_traces::{Exp, Typ};
use Exp::*;
use Typ::*;
use std::collections::HashMap;

/*


let o = { };

if (...) {
     o.x = "900";
     }

else {
    o.y = true;
}

typeinf(x * x) = (num, [x -> num])

typeinf(let y = x * x)

let x = "number";
let y = {
    let x = 20;
    50;
}

let x = "number";
let y = x * x;

typeinf([ x -> str ], let y = x * x, _)

typeinf([ x -> str ], x * x, _)

typeinf([ x -> str ], x, number)

let x = "foo";
let y = x + x;
let z = y + y;
let w = z + " hi";

let o1 = { };
o1.x = 20;
o1.y = 30;
o1.z = 50;




*/
