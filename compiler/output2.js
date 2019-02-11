const $T = require('./dist/runtime');

let $x = $T.bind($T.add($T.num(1), $T.num(3)));
let x = 1 + 3;

function F() {
    $T.if_($T.lt($x, $T.str('two')));
    if (x > 2) {
        $T.enterIf(true);;
        let $y = $T.bind($T.mul($T.num(2), $x));
        let y = 20 * x;
    }
    else {
        $T.enterIf(false);
        let $z = $T.bind($T.mul($T.num(5), $x));
        let z = 5 * x;
    }
    $T.exitIf();
    return 900;
}

F();