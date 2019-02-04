const $T = require('./dist/runtime');

let $x = $T.bind($T.add($T.num(1), $T.num(3)));
let x = 1 + 3;

let $y = $T.bind($T.lt($x, $T.str('two')));
let y = x < 'two';

$T.log();