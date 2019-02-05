const $T = require('./dist/runtime');


function F(x) {
    let $x = $T.input();
    $T.if_($T.lt($x, $T.num(20)));
    if (x < 20) {
        $T.enterIf(true);
        let $y = $T.bind($T.lt($x, $T.num(2)));
        let y = x < 2;
    } else {
        $T.enterIf(false);
    }
    $T.exitIf();
}

F(100);

$T.log();