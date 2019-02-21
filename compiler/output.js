let $T = require('./dist/runtime');

function F(x) {
  let _x = $T.input();

  let _y = $T.bind(_x);

  let y = x;
  $T.ifElse($T.lt(_y, $T.num(0)))

  if (y < 0) {
    $T.enterIf(true);
    $T.update(_y, $T.mul(_y, $T.neg($T.num(1))));
    y = y * -1;
  } else {
    $T.enterIf(false);
    $T.return_($T.add(_x, $T.num(200)))
    return x + 200;
  }

  $T.return_(_y)
  return y;
}

console.log(F(100));
$T.log();