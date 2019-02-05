import * as $T from '../ts/runtime';

test('please work', () => {
    let code = `
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
    F`;
    let f = eval(code);
    f(100);
    $T.log();
});